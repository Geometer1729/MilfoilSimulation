import matplotlib.pyplot as plt
import numpy as np
import haskelltalking as ht
from mpl_toolkits.mplot3d import Axes3D

file = open("depth.txt",)
out=""
for line in file:
    out += line
out=out[2:-2]
file.close()

x=ht.readnested(out)


x = np.transpose(x)
print(x)
xs = x[0]
ys = x[1]
zs = x[2]

fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')
ax.scatter(xs,ys,zs)

plt.show()
