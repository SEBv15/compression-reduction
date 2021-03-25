# Take raw data from different detectors, make it look like it could've come from our detector, and write it to a file
import numpy as np
from numba import jit

from utils import *

def main():
    for FILE in RAW_FILES:
        # Find the maximum pixel value in the entire dataset
        maximum = 0
        with open(f'C:/Users/Sebastian/Documents/argonne/fpga/compression_simulation/{FILE["path"]}', 'rb') as f:
            fi = FILE.copy()
            fi["bits_per_pixel"] = 0
            combine = min(FILE["width"], FILE["height"]) // 128
            for frame in get_raw_frames(f, x=FILE["width"]//2, y=FILE["height"]//2, combine=combine, **fi):
                tm = np.max(frame)
                if tm > maximum:
                    maximum = tm

        with open(f'C:/Users/Sebastian/Documents/argonne/fpga/compression_simulation/{FILE["path"]}', 'rb') as f:
            with open(f'data/{FILE["name"]}.bin', 'wb+') as outf:
                combine = min(FILE["width"], FILE["height"]) // 128
                for frame in get_raw_frames(f, x=FILE["width"]//2, y=FILE["height"]//2, combine=combine, reduce_custom=np.log2(maximum+1), **FILE):
                    frame = frame.astype(np.uint16)
                    outf.write(frame.tobytes())

if __name__ == "__main__":
    main()

