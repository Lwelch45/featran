#!/usr/bin/env python

import numpy as np

np.save(open("npy-double.npy", "w"), np.array([1, 2, 3, 4, 5], np.float64))
np.save(open("npy-float.npy", "w"), np.array([1, 2, 3, 4, 5], np.float32))
np.save(open("npy-long.npy", "w"), np.array([1, 2, 3, 4, 5], np.int64))
np.save(open("npy-int.npy", "w"), np.array([1, 2, 3, 4, 5], np.int32))
