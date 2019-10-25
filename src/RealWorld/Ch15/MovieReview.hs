module RealWorld.Ch15.MovieReview where

data MovieReview = MovieReview { revTitle :: String, revUser :: String, revReview :: String }

simpleReview :: [(String, Maybe String)] -> Maybe MovieReview
simpleReview map = do
  case lookup "title" map of
    Just (Just title@(_:_)) -> -- validating that the string is not empty
      case lookup "user" map of
        Just (Just user@(_:_)) ->
          case lookup "review" map of
            Just (Just review@(_:_)) -> Just (MovieReview title user review)
            Nothing -> Nothing
        Nothing -> Nothing
    Nothing -> Nothing

-- using bind
anotherBetterReview :: [(String, Maybe String)] -> Maybe MovieReview
anotherBetterReview map = lookup' "title" map >>= \title ->
                          lookup' "user" map >>= \user ->
                          lookup' "review" map >>= \review ->
                          return $ MovieReview title user review

-- using do notation
betterReview :: [(String, Maybe String)] -> Maybe MovieReview
betterReview map = do
  title  <- lookup' "title" map
  user   <- lookup' "user" map
  review <- lookup' "review" map
  return $ MovieReview title user review

-- using applicative syntax; a.k.a. generalized lifting
alsoBetterReview :: [(String, Maybe String)] -> Maybe MovieReview
alsoBetterReview map = MovieReview <$> mTitle <*> mUser <*> mReview
  where mTitle = lookup' "title" map
        mUser = lookup' "user" map
        mReview = lookup' "review" map

lookup' :: String -> [(String, Maybe String)] -> Maybe String
lookup' s map = case lookup s map of
                  Nothing -> Nothing
                  Just (Just s@(_:_)) -> Just s -- validate string is not empty
