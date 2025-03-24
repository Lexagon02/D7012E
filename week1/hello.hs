import Control.Monad.RWS (MonadState(put))
main = do
  putStrLn "Hello, everybody!"
  putStrLn ("Please look at my favorite odd numbers: " ++ show (filter odd [10..20]))

-- run with ghci
-- :load hello.hs
-- main
-- OR
-- ghci hello.hs
-- main

-- :q Quit
-- :l Load
-- :r Reload

-- it past run values

{-
multi line comments
comments
comments
-}

-- OBS run ghci in correct folder to acces all files


