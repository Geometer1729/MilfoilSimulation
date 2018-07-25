module Haskell.Params where


kwt :: Double -- light atenuation coefficient (water clarity)
kwt=0.5
d :: Double -- depth
d =4 -- length of the rope we have for measuring irradiance
mew0 :: Double
mew0 = 0.1
ps :: Double --density
ps=50
km :: Double --specific light attenuation coefficient for foil
km= 0.006
k1 :: Double --half saturation constant
k1=90
thetag :: Double --Arrhenius constant for growth
thetag=1.06
delta :: Double --loss rate
delta=0
tb :: Double --base temperature
tb = 10
mew1 :: Double -- carb use rate
mew1 = 0.4
chm :: Double -- density required to use carbohydrates at 50% capacity
chm = 50 --likely bad estimate
days :: Double
days = 100 -- likely bad estimate
lambda0::Double
lambda0= 0.01
thetar::Double
thetar = 1.072
tempAmp::Double
tempAmp = 25
tempPhase :: Double
tempPhase = 80
iradianceAmp :: Double
iradianceAmp = 50
iradiancePhase :: Double
iradiancePhase = 115
