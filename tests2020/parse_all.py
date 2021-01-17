#!/usr/bin/env python3

import glob
import os
import subprocess




if __name__ ==  "__main__":
    input_files = os.listdir('./')
    files = [file.split('.')[0] for file in glob.glob("*.scm")]
    for file in files:
        output = (subprocess.run(["./compare.sh", file], capture_output=True)).stdout
        if (output[-3:-1].decode("utf-8") != "#t"):
            print("Test {} failed!!\nrunnig compare output is:".format(file))
            print(output)
            break
        else:
            print("pass")
