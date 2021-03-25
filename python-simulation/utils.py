import numpy as np
from numba import jit
from math import log2
from typing import List

@jit(nopython=True)
def shuffle(pixels: np.ndarray, bits_per_pixel: int = 7) -> np.ndarray:
    """
    Shuffle the pixel data

    Arguments:
        pixels (np.ndarray): The pixel data
        bits_per_pixel (int): Number of bits in each pixel

    Returns:
        (np.ndarray): The shuffled pixels
    """
    out = np.zeros(bits_per_pixel, dtype=np.uint64)
    for i in range(0, bits_per_pixel):
        for j in range(0, pixels.shape[0]):
            out[i] <<= 1 # this actually doesn't work in plain python
            out[i] += ((pixels[pixels.shape[0] - j - 1]) & 2**i) != 0 # get the ith bit in every pixel and put it in the correspond output pixel
    return out

@jit(nopython=True)
def length_compress(shuffled_pixels: np.ndarray) -> np.ndarray:
    """
    Takes in SHUFFLED pixels and generates a header.

    Argument:
        shuffled_pixels (np.ndarray): The shuffled pixels to compress

    Returns:
        (np.ndarray): The same as the input array, but with the header appended to the front
    """
    length = 0
    for i in range(0, shuffled_pixels.shape[0]):
        if (shuffled_pixels[shuffled_pixels.shape[0] - i - 1] > 0):
            length = shuffled_pixels.shape[0] - i
            break
    
    return np.concatenate((np.asarray([length], dtype=np.uint64), shuffled_pixels[:length]))

@jit(nopython=True)
def length_shuffle_compress(pixels: np.ndarray, bits_per_pixel: int = 7) -> np.ndarray:
    """
    Shuffle and length compress the pixels.

    Arguments:
        pixels (np.ndarray): The pixels to compress
        bits_per_pixel (int): The number of bits in each pixel

    Returns:
        (np.ndarray): The shuffled and compressed data with the header as the first element
    """
    return length_compress(shuffle(pixels, bits_per_pixel=bits_per_pixel))

@jit(nopython=True)
def poisson_encode(pixels: np.ndarray) -> np.ndarray:
    """
    Takes a 1D array of 10-bit pixels and returns a poisson encoded 1D array of 7-bit pixels.

    Argument:
        pixels (np.ndarray): The list of 10-bit pixels

    Returns:
        (np.ndarray): A list of 7-bit pixels
    """
    for i, p in enumerate(pixels):
        if p < 16:
            pixels[i] = p
        elif p < 64:
            pixels[i] = (p >> 2) + 12
        elif p < 256:
            pixels[i] = (p >> 3) + 20
        else:
            pixels[i] = (p >> 4) + 36
    return pixels

def get_frames(path: str) -> np.ndarray:
    """
    Get the frames from a formatted data file that contains 16-bit uint 128x128 pixel frames.

    Argument:
        path (str): The path to the file to read

    Returns:
        (np.ndarray): The data from the file frame by frame
    """
    with open(path, 'rb') as file:
        while True:
            # Read a frame from the file
            bytess = file.read(128*128*2)
            frame = np.frombuffer(bytess, dtype=np.uint16)

            # Check if there is still data left
            if frame.shape[0] == 0:
                break

            # Yield the frame
            yield frame.reshape((128, 128)).astype(np.uint64)

@jit(nopython=True)
def reduce_data(data: np.ndarray, maxblocks: int = 128, data_len: int = 7) -> List[np.ndarray]:
    """
    Simulates a hierarchical reduction stage using the weird merge module.
    The input data should be formatted as a 2D array of compressor outputs with the header at index 0 and the data afterwards.

    Arguments:
        data (np.ndarray): A list of compressor outputs with the header first and the data after
        maxblocks (int): The maximum number of input elements into any reduction stage
        data_len (int): The maxmimum number of elements in the data from each compressor (= number of bits)

    Returns:
        (List[np.ndarray]): The 2-bit headers, 3-bit headers, and the data each as a numpy array
    """
    # Generate headers
    headers_2bit = np.zeros(data.shape[0], dtype=np.uint64)
    headers_3bit = np.zeros(data.shape[0], dtype=np.uint64)
    i3 = 0
    for i in range(data.shape[0]):
        headers_2bit[i] = data[i][0] if data[i][0] < 3 else 3
        if data[i][0] >= 3:
            headers_3bit[i3] = data[i][0]
            i3 += 1
    headers_3bit = headers_3bit[0:i3]

    # Reduce data
    n = data.shape[0]
    l = data_len
    m = 2
    data_s = data[:, 0].flatten()
    data_r = data[:, 1:].flatten()

    while n != 1:
        for i in range(n//2):
            pivot = int(max(0, data_s[2*i] + data_s[2*i+1] - l))
            data_r[int(2*l*i + data_s[2*i]):2*l*i + l] = data_r[2*l*i + l + pivot:int(2*l*i + l + l - data_s[2*i] + pivot)]
            data_s[i] = data_s[2*i] + data_s[2*i+1]

        l *= 2
        if (l > maxblocks):
            for i in range(n//2):
                if data_s[i] % m != 0:
                    data_s[i] += m - (data_s[i] % m)
            m *= 2

        n //= 2

    return [headers_2bit, headers_3bit, data_r[:data_s[0]]]

@jit(nopython=True)
def remove_negative_numbers(frame: np.ndarray) -> np.ndarray:
    """
    Takes in a 128x128 frame and sets all negative pixels to zero.

    Argument:
        frame (np.ndarray): The 128x128 frame

    Returns:
        (np.ndarray): The 128x128 frame without negative pixels
    """
    frame = frame.copy().ravel()
    for i in range(frame.shape[0]):
        if frame[i] < 0:
            frame[i] = 0
    frame = frame.reshape((128, 128))
    return frame

@jit(nopython=True)
def combine_pixels(frame: np.ndarray) -> np.ndarray:
    """
    Takes a square frame of any size >= 128x128 (preferably a multiple of 128x128), and sums adjacent pixels to return a 128x128 frame.

    Argument:
        frame (np.ndarray): The n x n pixel frame.

    Returns:
        (np.ndarray): The 128 x 128 pixel frame.
    """
    scale = frame.shape[0] // 128
    out = np.zeros((128, 128), dtype=frame.dtype)
    for i in range(128):
        for j in range(128):
            out[i][j] = np.sum(frame[scale*i:scale*i+scale, scale*j:scale*j+scale])

    return out

@jit(nopython=True)
def reduce_bits(frame: np.ndarray, cbits: float, tbits: float) -> np.ndarray:
    """
    Reduce the number of bits of a 128x128 pixel frame to the desired number.
    This is done by dividing every pixel value to fit within the given number of bits. 
    The remainder of the division is taken as a probability and rounding up/down is based on it.

    Parameters:
        frame (np.ndarray): A frame with 128x128 pixels
        cbits (float): The current number of bits (can be a non-int)
        tbits (float): The target number of bits (can be a non-int)

    Returns:
        (np.ndarray): The frame with less bits
    """
    div = 2**cbits / 2**tbits
    thresholds = np.random.randint(0, div, (128*128,))
    frame = frame.ravel()
    for i in range(128*128):
        frame[i] = frame[i] // div + (1 if (frame[i] % div) > thresholds[i] else 0)
        if frame[i] > 1023:
            frame[i] = 1023
    return frame.reshape((128, 128))

def get_raw_frames(file, width, height, x, y, combine = 1, dtype=np.uint32, bytes_per_pixel=4, bits_per_pixel=10, reduce_custom=None, **kwargs):
    """
    Function that reads the raw data files from various detectors and yields it frame by frame.
    """
    i = 0
    while True:
        bytess = file.read(bytes_per_pixel * width * height)
        dt = np.dtype(dtype)
        dt = dt.newbyteorder('<') # ensure correct endianness
        orig_frame = np.frombuffer(bytess, dtype=dt) # read data into numpy array

        if i in kwargs["bad_frames"]:
            i += 1
            continue

        if (len(orig_frame) < width * height):
            break
    
        frame = orig_frame.reshape((height, width))
        
        frame = frame[y-64*combine:y+64*combine, x-64*combine:x+64*combine]
        frame = combine_pixels(frame)


            
        frame = remove_negative_numbers(frame.astype(np.int32))
        frame = frame.astype(np.uint64)

        if (reduce_custom == None and bits_per_pixel + 2*log2(combine) > 10):
            frame = reduce_bits(frame, bits_per_pixel + 2*log2(combine), 10)
        elif reduce_custom != None and reduce_custom > 10:
            frame = reduce_bits(frame, reduce_custom, 10)

        for d in kwargs["dead_pixels"]:
            frame[d[1], d[0]] = 0

        yield frame.astype(np.uint64)

        i += 1

# Info about the raw data files to make working with them easier
RAW_FILES = [
    {
        "path": "scan144_1737_cropped_558x514.bin",
        "name": "ptychography",
        "width": 558,
        "height": 514,
        "bytes_per_pixel": 4,
        "bits_per_pixel": 14,
        "dtype": np.uint32,
        "stdpos": [260, 290],
        "dead_pixels": [],
        "bad_frames": []
    }, {
        "path": "A040_Latex_67nm_conc_025C_att0_Lq0_001_00001-01000_1556x516_uint8.bin",
        "name": "xpcs",
        "width": 1556,
        "height": 516,
        "bytes_per_pixel": 1,
        "bits_per_pixel": 10,
        "dtype": np.uint8,
        "stdpos": [778, 258],
        "dead_pixels": [],
        "bad_frames": [347, 815]
    }, {
        "path": "pilatus_image_1679x1475x300_int32.raw",
        "name": "pilatus",
        "width": 1475,
        "height": 1679,
        "bytes_per_pixel": 4,
        "bits_per_pixel": 14, # could actually be 14
        "dtype": np.int32,
        "stdpos": [1106, 1159],
        "dead_pixels": [[63, 22]],
        "bad_frames": []
    }
]