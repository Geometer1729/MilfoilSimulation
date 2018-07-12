import matplotlib.pyplot as plt
import numpy as np
from mpl_toolkits.mplot3d import Axes3D
file = open("data.txt",)
out= (''.join ( [c for line in file for c in line] [2:-2] ))
x = np.transpose ( [ [ float(p) for p in tup.split(',') ] for tup in out.split('),(')] )
print (x)
file.close()
plt.plot(x[0],x[1])
plt.show()
