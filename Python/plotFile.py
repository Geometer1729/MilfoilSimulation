import matplotlib.pyplot as plt
import numpy as np
import haskelltalking as ht
from mpl_toolkits.mplot3d import Axes3D

file = open("data.txt",)
out=""
for line in file:
    out += line
out=out[1:-1]
file.close()

x=ht.readarray(out)
x = np.array(x)
#x = np.transpose(x)

ht.plot(x)
