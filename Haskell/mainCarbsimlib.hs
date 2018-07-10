module Haskell.MainCarbsimlib where

import Haskell.Util.Interpelation

data Strain = Strain {
  mew0 :: Double
, ps :: Double --density
, km :: Double --specific light attenuation coefficient for foil
, k1 :: Double --half saturation constant
, thetag :: Double --Arrhenius constant for growth
, delta :: Double --loss rate
, tb :: Double --base temperature
, lambda :: Double -> Double --resperation rate as a function of temperature
, mew1 :: Double -- carb use rate
, chm :: Double -- density required to use carbohydrates at 50% capacity
}

data Lake = Lake {
  kwt :: Double -- light atenuation coefficient (water clarity)
, d :: Double -- depth
} deriving (Show)

data Season = Season{
  days :: Double
, temp :: Double -> Double
, i0 :: Double -> Double
}

data Enviornment = Enviornment {
  lake :: Lake
, strain :: Strain
, season :: Season
}


data Frame = Frame{
  t :: Double
, m :: Double
, dm :: Double
, c:: Double
, dc:: Double
}deriving (Show,Eq)

data Simulation = Simulation {
  frames :: [Frame]
, env :: Enviornment
} | DyStep {stepFrame :: Frame}

instance Eq Simulation where
  (==) (Simulation frames1 _ ) (Simulation frames2 _ ) = frames1 == frames2
  (==) (DyStep f1) (DyStep f2) = f1 == f2
  (==) _ _ = False



data GrowthFunc = GrowthFunc { growthFunc :: (Enviornment -> Double -> Double -> Double -> (Double,Double))}

sinusoid :: Double -> Double -> Double -> Double --usefull to build temp/iradiance functions
sinusoid amp phase input = amp * sin((pi*(input+phase))/360)**2

buildseason :: Double -> Double -> Double -> Double -> Double -> Season
buildseason days tempamp tempphase iradianceamp iradiancephase
    = Season days (sinusoid tempamp tempphase) (sinusoid iradianceamp iradiancephase)

lambdafunc :: Double -> Double -> Double -> Double -> Double --usefull to build resperation function
lambdafunc lambda0 thetar tb temp = lambda0 * ( thetar ** (temp - tb))

buildStrain :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double ->  Strain
buildStrain mew0 ps km k1 thetag delta lambda0 thetar tb mew1 chm
    = Strain mew0 ps km k1 thetag delta tb (lambdafunc lambda0 thetar tb) mew1 chm
--uniform biomass growth
ubgrowth :: GrowthFunc -- enviornment biomass carbs day -> change
ubgrowth = GrowthFunc growth
  where
    growth _ 0 _ _  = (0,0)
    growth env m c day = (dm , dc)
        where
            dc = carbflux
            dm =  mewt_ *m/(kwt_ * height + km_ *m)*log(( k1_ + im )/(k1_ + ird_))-m*(lambda_ + delta_ ) -carbflux
            carbflux = -mew1_ * c * (msat/(1+msat))
            msat = m/(chm strain_)
            strain_ = strain env
            lake_ = lake env
            season_ = season env
            k1_ = k1 strain_
            kwt_ = kwt lake_
            km_ = km strain_
            temp_ = temp season_
            mewt_ = mewt strain_ $ temp_ day
            height = min (d lake_) m/(ps strain_)
            d_ = d lake_
            delta_ = delta strain_
            lambda_ = lambda strain_ $ temp_ day
            i0_ = i0 season_ $ day
            im = i0_ * exp( -(kwt_)*(height - d_))
            ird_ = i0_ * exp(-(kwt_*d_+ km_*m))
            mew1_ = mew1 strain_
--Clear Watter Growth
cwgrowth :: GrowthFunc -- enviornment biomass carbs day -> change
cwgrowth = GrowthFunc growth
  where
    growth _ 0 _ _  = (0,0)
    growth env m c day = (dm , dc)
        where
            dc = dm*(0.06/0.94)
            dm = (mewt_/km_)*log((k1_+i0_)/(k1_+ird_)) * 0.94
            carbflux = -mew1_ * c * (msat/(1+msat))
            msat = m/(chm strain_)
            strain_ = strain env
            lake_ = lake env
            season_ = season env
            k1_ = k1 strain_
            kwt_ = kwt lake_
            km_ = km strain_
            temp_ = temp season_
            mewt_ = mewt strain_ $ temp_ day
            height = min (d lake_) m/(ps strain_)
            d_ = d lake_
            delta_ = delta strain_
            lambda_ = lambda strain_ $ temp_ day
            i0_ = i0 season_ $ day
            im = i0_ * exp( -(kwt_)*(height - d_))
            ird_ = i0_ * exp(-(kwt_*d_+ km_*m))
            mew1_ = mew1 strain_





mewt :: Strain -> Double -> Double --strain temp
mewt strn tmp = (mew0 strn)*(thetag strn)**(tmp-(tb strn))

simulate :: GrowthFunc -> Enviornment ->  Double -> Double -> Double -> Double -> Simulation
simulate growthF env step err initM initC = Simulation (framegen step err env growthF (Frame 0 initM dm initC dc)) env
  where (dm,dc) = (growthFunc growthF) env initM initC 0

framegen :: Double -> Double -> Enviornment -> GrowthFunc -> Frame -> [Frame]
framegen step err env growthF f
    |t_ < days_ = f : framegen step err env growthF (smartStep growthF f env adjStep err)
    |t_ == days_ = []
    |otherwise = error "framegen steped past season end!"
        where
          adjStep = min step (days_-t_)
          days_ = (days . season) env
          t_ = t f

smartStep :: GrowthFunc -> Frame -> Enviornment -> Double -> Double -> Frame
smartStep growthF f env maxStep mErr
  | err_<mErr = byhstp
  | otherwise = smartStep growthF f env (maxStep/2) (mErr/2)
  where
      halfstep = frameStep growthF env (maxStep/2)
      byhstp = (halfstep . halfstep) f
      bystp = frameStep growthF env maxStep f
      err_ = (((m byhstp)-(m bystp))^2+ ((c byhstp)-(c bystp))^2)**(0.5)--implement smarter norm here

frameStep ::  GrowthFunc -> Enviornment ->  Double -> Frame -> Frame
frameStep growthF env step f = Frame (t_+step) (m_+(k1+2*k2+2*k3+k4)/6) dm (c_+(l1+2*l2+2*l3+l4)/6) dc
  where
    t_ = t f
    m_ = m f
    c_ = c f
    growth = (growthFunc growthF)
    growth_ = growth env m_ c_ t_
    (dm,dc) = growth_
    (k1,l1) = scalepair step growth_
    (k2,l2) = scalepair step (growth env (m_ + k1/2) (c_ +l1/2) (t_+(step/2)))
    (k3,l3) = scalepair step (growth env (m_ + k2/2) (c_ + l2/2)(t_+(step/2)))
    (k4,l4) = scalepair step (growth env (m_ + k3) (c_ + l3) (t_+step))

scalepair:: Double -> (Double,Double) -> (Double,Double)
scalepair k (x,y) = (k*x,k*y)

siminterp :: Enviornment -> GrowthFunc -> Double -> Double -> Double -> Double -> [[Double]]
siminterp env growthF step err m_ c_ = interpelate x frames_ [m,c] [dm,dc] t
  where
    x = [0,1.. len]
    len = (days . season) env
    sim = simulate growthF env step err m_ c_
    frames_ = frames sim

wevil::Double->GrowthFunc->GrowthFunc
wevil w (GrowthFunc f) = GrowthFunc newfunc
  where
    newfunc::Enviornment->Double->Double->Double->(Double,Double)
    newfunc a b c d =  (\(x,y) -> (x-w,y)) (f a b c d)





-- Generate norwood lake enviornment
_kwt :: Double -- light atenuation coefficient (water clarity)
_kwt=0.5
_d :: Double -- depth
_d =0.5 --guess to model shalow watter case
_mew0 :: Double
_mew0 = 0.1
_ps :: Double --density
_ps=50
_km :: Double --specific light attenuation coefficient for foil
_km= 0.006
_k1 :: Double --half saturation constant
_k1=90
_thetag :: Double --Arrhenius constant for growth
_thetag=1.06
_delta :: Double --loss rate
_delta=0
_tb :: Double --base temperature
_tb = 10
_mew1 :: Double -- carb use rate
_mew1 = 0.4
_chm :: Double -- density required to use carbohydrates at 50% capacity
_chm = 50 --likely bad estimate
_days :: Double
_days = 100 -- likely bad estimate
_lambda0::Double
_lambda0= 0.01
_thetar::Double
_thetar = 1.072

{-
norwood = Lake 0.5 0.5
genStrain = buildStrain 0.1 50 0.006 90 1.06 0 0.01 1.072 10
buildStrain             mew0 ps km k1 thetag delta lambda0 thetar tb
genSeason = buildseason 200 25 (-20) 50 15
buildseason days tempamp tempphase iradianceamp iradiancephase
norwoodEnv = Enviornment norwood genStrain genSeason
-}


norwood = Lake _kwt _d
genStrain = buildStrain _mew0 _ps _km _k1 _thetag _delta  _lambda0 _thetar _tb _mew1 _chm
genSeason = buildseason _days 25 80 50 115
norwoodEnv = Enviornment norwood genStrain genSeason
