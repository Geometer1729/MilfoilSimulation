import matplotlib.pyplot as plt
import numpy as np
from mpl_toolkits.mplot3d import Axes3D
file = open("data.txt",)
out= (''.join ( [c for line in file for c in line] [3:-3] ))
x = np.array ([ [ [ float(p) for p in tup.split(',') ] for tup in grid.split('],[')] for grid in out.split(']],[[')])
file.close()
fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')
ax.plot_wireframe(x[0],x[1],x[2])
plt.show()
