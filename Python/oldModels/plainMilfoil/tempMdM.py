import haskelltalking as ht
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D


def tempMdM(inputs):
    path = "../Haskell"
    file = "tempMdM.exe"
    return ht.readtuplearray(ht.stripnoise(ht.runexe(inputs,path,file)))


X=tempMdM([input("min milfoil: "),input("milfoil step: "),input("max milfoil: "),input("min time: "),input("time step: "),input("end time: ")])
X=np.array(X)
X=np.transpose(X)


fig = plt.figure("temperaute, Milfoil, growthrate")
ax = fig.add_subplot(111, projection='3d')
ax.set_xlabel("temperature")
ax.set_ylabel("Milfoil")
ax.set_zlabel("growth rate")
ax.scatter(X[2],X[1],X[0])
plt.show()
