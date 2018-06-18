import haskelltalking as ht
import matplotlib as plot
import numpy as np

def sim(step,err,m,c):
    inputs= [step,err,m,c]
    path = "../Haskell"
    file = "gcomp.exe"
    str=ht.runexe(inputs,path,file)
    return (str.split('[[')[1]).split(']]')[0]

X=sim(input("max step: "),input("max err: "),input("starting milfoil: "),input("starting carbohydrates: "))
X= np.transpose(np.array(ht.readnested(X)))
ht.twoplot(X[0],X[1])


#ht.plot(sim(input("max step: "),input("max err: "),input("starting milfoil: "),input("starting carbohydrates: ")))
