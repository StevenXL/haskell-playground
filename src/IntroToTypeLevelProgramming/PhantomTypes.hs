{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module IntroToTypeLevelProgramming.PhantomTypes where

import           Test.Hspec

newtype Distance a = Distance { unDistance :: Double } deriving (Eq, Show, Num)

data Mile

data Kilometer

milesToKilometers :: Distance Mile -> Distance Kilometer
milesToKilometers distanceInMiles = Distance (unDistance distanceInMiles * 1.60934)

kilometersToMiles :: Distance Kilometer -> Distance Mile
kilometersToMiles distanceInKilometers = Distance (unDistance distanceInKilometers * 0.621371)

class Convert a b where
  convert :: Distance a -> Distance b

-- instance Convert (Distance Mile) (Distance Kilometer); we'd be passing
instance Convert Mile Kilometer where
  convert = milesToKilometers

instance Convert Kilometer Mile where
  convert = kilometersToMiles

class Add a b where
  addDistance :: Distance a -> Distance b -> Distance b

instance Add Mile Kilometer where
  addDistance distanceInMiles distanceInKilometers = convert distanceInMiles + distanceInKilometers

instance Add Kilometer Mile where
  addDistance distanceInKilometers distanceInMiles = convert distanceInKilometers + distanceInMiles

instance Add Mile Mile where
  addDistance = (+)

instance Add Kilometer Kilometer where
  addDistance = (+)

main :: IO ()
main = hspec $ do
  it "converts from miles to kilometers" $ do
    let distanceInMiles = Distance 10 :: Distance Mile
    (convert distanceInMiles :: Distance Kilometer) `shouldBe` Distance 16.0934
  it "converts from kilometers to miles" $ do
    let distainceInKilometers = Distance 10 :: Distance Kilometer
    (convert distainceInKilometers :: Distance Mile) `shouldBe` Distance 6.21371
  it "can add miles to kilometers" $ do
    let distanceInKilometers = Distance 10 :: Distance Kilometer
        distanceInMiles = Distance 10 :: Distance Mile
    (addDistance distanceInMiles distanceInKilometers) `shouldBe` (Distance 26.0934 :: Distance Kilometer)
  it "can add kilometers to miles" $ do
    let distanceInKilometers = Distance 10 :: Distance Kilometer
        distanceInMiles = Distance 10 :: Distance Mile
    (addDistance distanceInKilometers distanceInMiles) `shouldBe` (Distance 16.21371 :: Distance Mile)
