import matplotlib.pyplot as plt
import numpy as np
import haskelltalking as ht
from mpl_toolkits.mplot3d import Axes3D

file = open("SeasonCarbMap.txt",)
out=""
for line in file:
    out += line
out=out[2:-2]
file.close()

x=ht.readnested(out)
x = np.array(x)
x = np.transpose(x)

Axes3D.scatter(x[0],x[1],x[2])
