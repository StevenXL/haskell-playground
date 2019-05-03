module RealWorld.Ch15.MovieReviewTwo where

import RealWorld.Ch15.NonEmptyString (NonEmptyString, mkNonEmptyStr)

data MovieReviewTwo = MovieReviewTwo
  { revTTitle :: NonEmptyString
  , revTUser :: NonEmptyString
  , revTReview :: NonEmptyString
  }

simpleReview :: [(String, Maybe String)] -> Maybe MovieReviewTwo
simpleReview map = do
  case lookup "title" map of
    Just (Just title)->
      case mkNonEmptyStr title of
        Just title' ->
          case lookup "user" map of
            Just (Just user) ->
              case mkNonEmptyStr user of
                Just user' ->
                  case lookup "review" map of
                    Just (Just review) ->
                      case mkNonEmptyStr review of
                        Just review' -> return $ MovieReviewTwo title' user' review'
                        Nothing -> Nothing
                    Nothing -> Nothing
                Nothing -> Nothing
            Nothing -> Nothing
        Nothing -> Nothing
    Nothing -> Nothing

-- using bind
anotherBetterReview :: [(String, Maybe String)] -> Maybe MovieReviewTwo
anotherBetterReview map = lookup' "title" map >>= \title ->
                          lookup' "title" map >>= \user ->
                          lookup' "title" map >>= \review ->
                          return $ MovieReviewTwo title user review

lookup' :: String -> [(String, Maybe String)] -> Maybe NonEmptyString
lookup' s map = lookup s map >>= id >>= mkNonEmptyStr
