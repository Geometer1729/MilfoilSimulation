module SimLibs.Mainsimlib where

import Interpelation.Interpelation

data Strain = Strain {
  mew0 :: Double
, ps :: Double --density
, km :: Double --specific light attenuation coefficient for foil
, k1 :: Double --half saturation constant
, thetag :: Double --Arrhenius constant for growth
, delta :: Double --loss rate
, tb :: Double --base temperature
, lambda :: Double -> Double --resperation rate as a function of temperature
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
}deriving (Show)

data Simulation = Simulation {
  frames :: [Frame]
, env :: Enviornment
}

sinusoid :: Double -> Double -> Double -> Double --usefull to build temp/iradiance functions
sinusoid amp phase input = amp * sin((pi*(input+phase))/360)**2

buildseason :: Double -> Double -> Double -> Double -> Double -> Season
buildseason days tempamp tempphase iradianceamp iradiancephase
    = Season days (sinusoid tempamp tempphase) (sinusoid iradianceamp iradiancephase)

lambdafunc :: Double -> Double -> Double -> Double -> Double --usefull to build resperation function
lambdafunc lambda0 thetar tb temp = lambda0 * ( thetar ** (temp - tb))

buildStrain :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Strain
buildStrain mew0 ps km k1 thetag delta lambda0 thetar tb
    = Strain mew0 ps km k1 thetag delta tb (lambdafunc lambda0 thetar tb)

growth :: Enviornment -> Double -> Double -> Double -- enviornment biomass day -> growth
growth _ 0 _ = 0
growth env m day
  = mewt_ *m/(kwt_ * height + km_ *m)*log(( k1_ + im )/(k1_ + ird_))-m*(lambda_ + delta_ )
    where
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

mewt :: Strain -> Double -> Double --strain temp
mewt strn tmp = (mew0 strn)*(thetag strn)**(tmp-(tb strn))

simulate :: Enviornment -> Double -> Double -> Double -> Simulation
simulate env step err initM = Simulation (framegen step err env (Frame 0 initM (growth env initM 0))) env

framegen :: Double -> Double -> Enviornment -> Frame -> [Frame]
framegen step err env f
    |t_ < (days (season env)) = f : framegen step err env (smartStep f env step err)
    |t_ >= (days (season env)) = []::[Frame]
        where
          growth_ = growth env m_ t_
          m_ = m f
          t_ = t f

smartStep :: Frame -> Enviornment -> Double -> Double -> Frame
smartStep f env maxStep mErr
  | err<mErr = byhstp
  | otherwise = smartStep f env (maxStep/2) (mErr/2)
  where
      halfstep = frameStep env (maxStep/2)
      byhstp = (halfstep . halfstep) f
      bystp = frameStep env maxStep f
      err = (m byhstp)-(m bystp)

frameStep ::  Enviornment ->  Double -> Frame -> Frame
frameStep env step f = Frame (t_+step) (m_+(k1+2*k2+2*k3+k4)/6) (growth_)
  where
    t_ = t f
    m_ = m f
    dm_ = dm f
    growth_ = growth env m_ t_
    k1 = step*growth_
    k2 = step*(growth env (m_ + k1/2) (t_+(step/2)))
    k3 = step*(growth env (m_ + k2/2) (t_+(step/2)))
    k4 = step*(growth env (m_ + k3) (t_+step))

siminterp :: Enviornment ->  Double -> Double -> Double -> [Double]
siminterp env  step err m_ = fmap head (interpelate x (fmap t frames_ ) frames_ [m] [dm] t)
  where
    x = [1,2.. len]
    len = (days . season) env
    sim = simulate env step err m_
    frames_ = frames sim

-- Generate norwood lake enviornment
norwood = Lake 0.5 0.5
genStrain = buildStrain 0.1 50 0.006 90 1.06 0 0.01 1.072 10
genSeason = buildseason 200 25 (-20) 50 15
norwoodEnv = Enviornment norwood genStrain genSeason
