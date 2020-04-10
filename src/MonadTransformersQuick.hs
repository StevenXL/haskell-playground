{-# LANGUAGE InstanceSigs  #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

module MonadTransformersQuick where

import Control.Monad.Trans.Class (MonadTrans (..))

main :: IO ()
main = do
  print "Running stack 'MaybeT IO String'"
  runMaybeT maybeTSucess >>= print
  runMaybeT maybeTFail >>= print
  print "Running stack 'WriterT [String] IO Int'"
  runWriterT writerTComp >>= print
  print "Running stack 'WriterT [String] (MaybeT IO) Int'"
  runMaybeT (runWriterT writerTMaybeT) >>= print
  print "Running stack 'MaybeT (WriterT [String] IO) Int'"
  runWriterT (runMaybeT maybeTWriterT) >>= print

-- TAKEAWAYS
-- A) We were able to define the MaybeT and WriterT monad transformers
-- B) We were able to create a monadic stack where order DOES matter
-- C) We were able to combine two transformers to get two effects
-- D) Ex 1: Assume we have a value komp :: WriterT w (MaybeT m) a
--       runWriterT komp :: MaybeT m (a, w)
--       runMaybeT (runWriterT komp) :: m (Maybe (a, w))
-- E) Ex 2: Assume we have a value komp :: MaybeT (WriterT w m) a
--       runMaybeT komp :: WriterT w m (Maybe a)
--       runWriterT (runMaybeT komp) :: m (Maybe a, w)
-- F) Notice that in Ex 1, when we fail, we lose all of our logs - the base
--    monad will return a Nothing value. In Ex 2, when we fail, the base monad
--    will return a value of (Nothing, w) -- i.e., we keep our logs.


maybeTFail :: MaybeT IO String
maybeTFail = do
  lift (print "I am the failedComp")
  fail "Steven"
  return "Steven" -- because a previous computation failed, this will not be returned

maybeTSucess :: MaybeT IO String
maybeTSucess  = do
  lift (print "I am the successfulComp")
  return "Steven"

writerTComp :: WriterT [String] IO Int
writerTComp = do
  tell ["First Log"]
  tell ["Second Log"]
  pure 2

writerTMaybeT :: WriterT [String] (MaybeT IO) Int
writerTMaybeT = do
  tell ["Running writerTMaybeT"]
  lift $ fail "I failed"
  pure 1

maybeTWriterT :: MaybeT (WriterT [String] IO) Int
maybeTWriterT = do
  lift $ tell ["Running maybeTWriterT"]
  fail "Failed"
  pure 1

-- START MaybeT Monad Transformer
newtype MaybeT m a = MaybeT { unMaybeT :: m (Maybe a) }

runMaybeT :: MaybeT m a -> m (Maybe a)
runMaybeT = unMaybeT

instance Functor m => Functor (MaybeT m) where
  fmap :: (a -> b) -> MaybeT m a -> MaybeT m b
  fmap f m = MaybeT komp
    where komp = fmap f <$> runMaybeT m

instance Monad m => Applicative (MaybeT m) where
  pure :: a -> MaybeT m a
  pure = MaybeT . pure . pure

  (<*>) :: MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
  m <*> s = MaybeT komp
    where komp = do
            mF <- runMaybeT m
            mA <- runMaybeT s
            pure (mF <*> mA)

instance Monad m => Monad (MaybeT m) where
  return :: a -> MaybeT m a
  return a = MaybeT (pure $ return a)

  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  m >>= k = MaybeT komp
    where komp = runMaybeT m >>= \case
                                     Just a -> runMaybeT (k a)
                                     Nothing -> pure Nothing

  fail :: String -> MaybeT m a
  fail _ = MaybeT $ pure Nothing

instance MonadTrans MaybeT where
  lift :: Monad m => m a -> MaybeT m a
  lift m = MaybeT (fmap Just m)

-- END MaybeT Monad Transformer

-- START WriterT Monad Transformer
newtype WriterT w m a = WriterT { unWriterT :: m (a, w) }

-- HELPER FUNCTIONS
runWriterT :: WriterT w m a -> m (a, w)
runWriterT = unWriterT

tell :: Applicative m => w -> WriterT w m ()
tell w = WriterT $ pure ((), w)

instance Functor m => Functor (WriterT w m) where
  fmap :: (a -> b) -> WriterT w m a -> WriterT w m b
  fmap f w = WriterT komp
    where komp = fmap (\(a, w) -> (f a, w)) (runWriterT w)

instance (Monad m, Monoid w) => Applicative (WriterT w m) where
  pure :: a -> WriterT w m a
  pure a = WriterT $ pure (a, mempty)

  (<*>) :: WriterT w m (a -> b) -> WriterT w m a -> WriterT w m b
  w <*> w' = WriterT komp
    where komp = do
            (f, log) <- runWriterT w
            (a, log') <- runWriterT w'
            pure (f a, log <> log')

instance (Monad m, Monoid w) => Monad (WriterT w m) where
  return :: a -> WriterT w m a
  return = pure

  (>>=) :: WriterT w m a -> (a -> WriterT w m b) -> WriterT w m b
  w >>= k = WriterT komp
    where komp = do
            (a, log) <- runWriterT w
            (b, log') <- runWriterT (k a)
            pure (b, log <> log')

instance Monoid w => MonadTrans (WriterT w) where
  lift :: Monad m => m a -> WriterT w m a
  lift m = WriterT $ fmap (,mempty) m
-- -- END WriterT Monad Transformer
