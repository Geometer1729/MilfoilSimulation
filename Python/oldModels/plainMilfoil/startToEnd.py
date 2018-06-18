import haskelltalking as ht


def ste(inputs):
    path = "../Haskell"
    file = "startToEndfunc.exe"
    return ht.readarray(ht.stripnoise(ht.runexe(inputs,path,file)))
X=ste([input("err: "),input("min: "),input("max: "),input("step: ")])
ht.plot(X)

#plot the derivitive
"""
Y=[]
xl=X[0]
for x in X:
    Y.append(xl-x)
    xl=x

ht.plot(Y)
"""
