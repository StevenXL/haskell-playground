module Reader where

-- Ingredients for a monad:
-- 1) Data Type
-- 2) Helper Function(s)
-- 3) Run Function(s)
-- DATA TYPE
newtype Reader r a = Reader
  { unReader :: r -> a
  }

-- HELPER FUNCTIONS
ask :: Reader r r
ask = Reader id

local :: (r -> r) -> Reader r a -> Reader r a
local modifier reader = Reader $ \r -> (runReader reader) (modifier r)

asks :: (r -> a) -> Reader r a
asks = Reader

runReader :: Reader r a -> r -> a
runReader m r = unReader m r
