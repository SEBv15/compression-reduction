# Python Simulation

Estimate the compression ratios using python and data from other detectors.

## `convert_data.py`

The `convert_data.py` takes the data files defined in `utils.py` and turns them into 10-bit 128x128 pixel data files. It does this by combining adjacent pixels and then scaling everything so the maximum value in the whole dataset is 1023.

## `estimate_compression.ipynb`

This is a Jupyter notebook that does some analysis on the files generated by `convert_data.py`. Those preconverted files can also be found [here](https://anl.box.com/s/folizdpti1i95oz7yxclvheoysq5fmhf) (they need to be placed in a `data/` folder in this directory). 