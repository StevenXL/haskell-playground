{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ToOvercome.ReturnAFunction where

import Control.Monad (forM)
import System.Random (randomIO)

-- The immediate goal here was to write a program that would read data in from
-- the a database, convert the data into a new schema, and save the new data to
-- a separate database.
--
-- More generally, the technique discussed here is meant to help push SIDE
-- EFFECTS to the edge of our system.
class LoadData i where
  load :: IO [i]

class ConvertData i o | i -> o where
  convert :: i -> o

class SaveData o where
  save :: [o] -> IO ()

migrate ::
     forall i o. (LoadData i, ConvertData i o, SaveData o)
  => IO ()
migrate = do
  old <- load @i
  let new = map convert old
  save new

-- Now, we need to generate a random UUID in order to convert the old format
-- into the new format. Instead of polluting our ConvertData typeclass with IO,
-- we return a function that can be applied to a UUID. This has the benefit of
-- pushing the side effect of generating a new UUID to teh edges of our type
-- system.
newtype NeedsUUID = NeedsUUID
  { giveUUID :: UUID -> New
  }

class ConvertData' i o | i -> o where
  convert' :: i -> NeedsUUID

type UUID = Int

data Old =
  Old Int
      Int

data New =
  New UUID
      Int
      Int

-- now, we create an instance of SaveData which already did IO:
instance SaveData NeedsUUID where
  save needsUUIDs = do
    values <-
      forM needsUUIDs $ \needs -> do
        newUUID <- randomIO
        return $ giveUUID needs newUUID
    saveNewData values

saveNewData :: [New] -> IO ()
saveNewData _ = return ()
