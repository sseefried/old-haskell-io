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

{-

map f [] = []
map f (x:xs) = f x : map f xs


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
