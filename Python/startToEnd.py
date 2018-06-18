import haskelltalking as ht
import matplotlib as plot
import numpy as np

def ste(err,max):
    inputs= [err,max]
    path = "../Haskell"
    file = "preflowering.exe"
    return ht.runexe(inputs,path,file)


ht.plot(ht.readarray(ht.stripnoise(ste(input("err: "),input("max: ")))))
