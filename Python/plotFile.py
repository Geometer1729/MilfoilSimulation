import matplotlib.pyplot as plt
import numpy as np
from mpl_toolkits.mplot3d import Axes3D
file = open("data.txt",)
out= (''.join ( [c for line in file for c in line] [1:-1] ))
x = [float(x) for x in out.split(',')]
file.close()
plt.plot(range(0,len(x)),x)
plt.show()
