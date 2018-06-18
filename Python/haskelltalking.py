import os
import subprocess
import matplotlib.pyplot as plt

def runexe(inputs,path,file):
    os.chdir(path)
    return str( subprocess.check_output([file]+[str(x) for x in inputs]))

def readarray(read):
    return [float(x) for x in read.split(',')]

def stripnoise(str):
    return str[3:-6]

def plot(X):
    plt.plot(range(0,len(X)),X)
    plt.show()

def readtuplearray(read):
    return [readtuple(x) for x in read[1:-2].split('),(')]

def readnested(read):
    return [readarray(x) for x in read.split('],[')]


def readtuple(str):
    return [float(x) for x in str.split(',')]

def twoplot(X1,X2):
    plt.plot(range(0,len(X1)),X1)
    plt.plot(range(0,len(X2)),X2)
    plt.show()
