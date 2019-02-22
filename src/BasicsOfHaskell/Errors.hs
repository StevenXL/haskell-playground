module BasicsOfHaskell.Errors where

data DivError =
  DivisionByZero

type Numerator = Float

type Denominator = Float

-- This is my attempt at a Short, Self-Contained Example of Ch.10 of Bartosz
-- Milewski's Basics of Haskell
unSafeDiv :: Float -> Float -> Float
unSafeDiv numerator denominator = numerator / denominator

-- The code here compiles just fine, but it is a run-time error.
runTimeError :: Float
runTimeError = unSafeDiv 1 0

-- What's the big deal? Under particular circumstances, throwing an error is
-- valid, and if the programmer remembers to catch the error, everything is
-- well. But in order to catch the error, the programmer has to know that a
-- piece of code might result in an error. Not only do you have to know that a
-- piece of code might result in an error, but you also have to remember to
-- catch it. In other languages, too much of the problem is shifted back to
-- falible humans.
-- A separate solution in Haskell is to encode in the type system that an error
-- can occur. The two easiest ways to do this is to use the `Maybe` or `Either`
-- type constructors. We're going to use `Either` because we can encode what the
-- error was:
safeDiv :: Float -> Float -> Either DivError Float
safeDiv _ 0 = Left DivisionByZero
safeDiv numerator denominator = Right (numerator / denominator)

-- Now, we have encoded the notion of failure / exception into our type system.
-- What does this buy us? The programmer knows, by looking at the type of of our
-- function, that it can potentially error. Furthermore, the programmer is
-- practically forced by the type system to "catch" the error. So now we have a
-- computation that can fail, but it does so in a completely pure way.
-- This presents a problem for us. Let's say we have a function that takes four
-- floats, uses the first as a numerator, and the other three as denominators:
crazyDiv ::
     Numerator
  -> Denominator
  -> Denominator
  -> Denominator
  -> Either DivError Float
crazyDiv n d1 d2 d3 =
  case safeDiv n d1 of
    Left msg -> Left msg
    Right n1 ->
      case safeDiv n1 d2 of
        Left msg -> Left msg
        Right n2 -> safeDiv n2 d3

-- Because safeDiv can fail, we have to continue checking for failure. The good
-- news is that we are forced to catch all potential failures. The bad news is
-- that this code is not easy to reason about. The code is not about error
-- catching, it is about dividing, but I can't tell that that is the case
-- without reading through the code in "slow motion".
-- However, there is a pattern here, and the pattern is that if the computation
-- has failed, then simply return the failure. If the computation succeeds, make
-- the results of the computation available further down the line. We can
-- abstract this out into a function:
bindE :: Either e a -> (a -> Either e b) -> Either e b
bindE (Left msg) _ = Left msg
bindE (Right f) k = k f

-- With bindE, we can re-write our code from above:
lessCrazy ::
     Numerator
  -> Denominator
  -> Denominator
  -> Denominator
  -> Either DivError Float
lessCrazy n d1 d2 d3 =
  safeDiv n d1 `bindE` (\n1 -> safeDiv n1 d2 `bindE` (\n2 -> safeDiv n2 d3))

-- bindE is actually is actually the `bind` method from the Monad typeclass for
-- the `Either a` instance. This means that we can use do notation:
notCrazy ::
     Numerator
  -> Denominator
  -> Denominator
  -> Denominator
  -> Either DivError Float
notCrazy n d1 d2 d3 = do
  n1 <- safeDiv n d1
  n2 <- safeDiv n d2
  safeDiv n2 d3

-- This doesn't look too bad at all. We have almost made a round-trip back to
-- where we started. So the question remains, what have we gained? Well, we have
-- modeled the idea of failure - a.ka. "notion of computation" - in a pure way.
-- This means we gain all the benefits of pure functions; namely local reasoning
-- and composition. Furthermore, it is all typechecked. Even though we don't
-- "see" the check for errors on L87 and L88, we are in fact checking for
-- errors, and the type system ensures that we cannot forget to check for
-- errors.
-- example of composing not crazy:
crazy ::
     Numerator
  -> Denominator
  -> Denominator
  -> Denominator
  -> Either DivError Float
crazy n d1 d2 d3 = notCrazy n d1 d2 d3 `bindE` notCrazy n d1 d2
-- because of our definition of bindE, if the first notCrazy fails, the second
-- will never be called. If the first notCrazy succeds, the float will be used
-- as the "d3" in the second notCrazy.
