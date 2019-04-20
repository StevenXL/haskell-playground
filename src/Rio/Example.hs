{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Rio.Example where

import RIO
import Prelude (print)
import qualified Control.Monad.Writer.Lazy as W
import qualified Control.Monad.Reader as R

data Repo = Repo { repoName :: String }

data Auth = Auth

class MonadGitHub m where
  getRepos :: Auth -> m [Repo]
  deleteRepo :: Auth -> Repo -> m ()

class HasGitHub env where
  authL :: Lens' env Auth

deleteRepos :: (MonadReader env m, HasGitHub env, MonadGitHub m) => m ()
deleteRepos = do
  auth <- view authL
  repos <- getRepos auth
  forM_ repos $ \repo -> deleteRepo auth repo

----- TEST CODE

type Test = R.ReaderT Auth (W.Writer [String])

instance HasGitHub Auth where
  authL = lens id (\x _ -> x)

instance MonadGitHub Test where
  getRepos _ = return [Repo "1", Repo "2"]
  deleteRepo _ repo = W.tell ["deleted repo " ++ repoName repo]

main :: IO ()
main = do
  let logs = W.execWriter $ R.runReaderT (deleteRepos :: Test ()) Auth
  if logs == ["deleted repo 1", "deleted repo 2"]
  then print "success"
  else print "failure"
