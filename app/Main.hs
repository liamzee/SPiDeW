module Main where

import Spidew
import System.Console.Haskeline
import Control.Monad.State (StateT(runStateT))

main = startup >> runInputT defaultSettings (runStateT takeInputLoop "")

startup :: IO ()
startup = putStrLn "Welcome to SPiDew! Input your API key via \"key <key>\" and look up a \n\
    \stock price via \"show <symbol>\". \"watch <symbol>\" adds to your watch-list, and \n\
    \\"unwatch <symbol>\" removes it from your watchlist. \"watchlist\" shows the prices on\
    \your entire watchlist."
