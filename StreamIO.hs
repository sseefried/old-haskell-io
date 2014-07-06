{-# LANGUAGE RecursiveDo #-}
module StreamIO where

import System.IO.Unsafe


data Request = GetChar
             | PutStr String

data Response = Char Char
              | Success

type Behaviour = [Response] -> [Request]

streamIOTest :: Behaviour
streamIOTest ~(Success : ~(Char c : ~(Success : ~_))) =
  [ PutStr "Enter a character: ",
    GetChar,
    PutStr ("\nThe character you typed was " ++ show c ++ "\n")
  ]

--
-- I'm not sure about this but I think the reason that the type signature
-- is [Response] -> [Request] and not the other way around is that
-- it solves the problem of how to pattern match against the Response to
-- a previous Request. Let's just have a look at streamIOTest above.
--
-- If you tried to write it as a function with type [Request] -> [Response]
-- things quickly go awry!
--
-- How do you know the value of 'c' for the third request?
--
-- e.g.
--
-- streamIOTest [ PutStr "Enter a character: ",
--                GetChar,
--                PutStr ("\nThe character you typed was " ++ show c ++ "\n")
--               ] = ...
--


{-

Could you write it this way?

f1 [PutStr "enter a char"] = [Success]
f2 [GetChar] = [Char c] -- again, where does the 'c' come from ?

-}

exec :: Request -> IO Response
exec req = case req of
             PutStr s -> putStr s >> return Success
             GetChar  -> getChar >>= \c -> return (Char c)


{- "Lazy evaluation allows a program to generate a request prior to processing any responses" -}
runB :: Behaviour -> IO ()
runB f = do
  let reqs  = f resps
      resps = map (unsafePerformIO . exec) reqs
  foldr seq (return ()) resps -- force each response to be evaluated with 'seq'

-----------------------------------------------------------------------

runB2 :: Behaviour -> IO ()
runB2 f = do
  let (req : reqs) = f (resp : resps)
      resp         = unsafePerformIO . exec $ req
      resps        = map (unsafePerformIO . exec) reqs
  resp `seq` foldr seq (return ()) resps

-----------------------------------------------------------------------

mapEmmy :: Monad m => (a -> m b) -> [a] -> m [b]

mapEmmy f [] = return []
mapEmmy f ~(a: ~as) = do
    b  <- f a
    bs <- mapEmmy f as
    return (b:bs)

--
-- runB' does not currently work
--

runB' :: Behaviour -> IO ()
runB' f = do
  rec resps <- mapEmmy exec (f resps)
  foldr seq (return ()) resps -- force each response to be evaluated with 'seq'

  where
    exec req = case req of
                 PutStr s -> putStr s >> return Success
                 GetChar  -> getChar >>= \c -> return (Char c)
