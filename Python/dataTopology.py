import haskelltalking as ht
import os
import numpy as np

def getComplex(m,c,t,epsilon):
    inputs= [m,c,t,epsilon]
    path = "../Haskell"
    file = "dataTopology.exe"
    str=ht.runexe(inputs,path,file).split('[')[1].split(']')[0]
    if str == '':
        return []
    return readTupleIntsArray(str)

def readTupleIntsArray(read):
    return [readtupleints(x) for x in read[1:-2].split('),(')]

def readtupleints(str):
    return [int(x) for x in str.split(',')]

m=input("m: ")
c=input("c: ")
t=input("t: ")


os.chdir("../python")
file = open("data.txt",'w')
file.write("Generated with\n")
file.write("m= "+str(m)+'\n')
file.write("c= "+str(c)+'\n')
file.write("t= "+str(t)+'\n')
for epsilon in np.arange(1,2,0.05):
    file.write('\n epsilon ='+str(epsilon)+'\n')
    file.write(str(getComplex(m,c,t,epsilon)))
    print(str((epsilon-1)*100+5)+'%')
file.close()
print("complete")
