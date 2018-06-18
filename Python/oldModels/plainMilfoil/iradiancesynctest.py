import haskelltalking as ht
import numpy as np
import matplotlib.pyplot as plt

def run(center,range,res):
    inputs= [center,range,res]
    path = "../Haskell"
    file = "iradianceSyncTest.exe"
    return ht.readarray(ht.stripnoise(ht.runexe(inputs,path,file)))

center=input("center :")
range=input("range :")
res = input("res :")
Y=run(center,range,res)
Y=np.array(Y)
center=float(center)
range=float(range)
res=float(res)
X=np.arange(center-range,center+range+res,res)
plt.plot(X,Y)
plt.show()
