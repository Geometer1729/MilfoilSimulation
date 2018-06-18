import haskelltalking as ht

def sim(step,err,m):
    inputs= [step,err,m]
    path = "../Haskell"
    file = "sim.exe"
    return ht.readarray(ht.stripnoise(ht.runexe(inputs,path,file)))

step=input("max step: ")
err=input("max err: ")
m=input("starting biomass: ")
ht.plot(sim(step,err,m))
