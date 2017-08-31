{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Garlic
( 
    garlic
)
where

import Control.Exception (catch, finally)
import Control.Monad.Reader
import Data.Text (unpack)
import Database.Persist.Sqlite hiding (Connection)
import Database.Sqlite
import GI.Gtk (GError, gerrorMessage, init)
import Prelude hiding (init)
import Reactive.Banana.Frameworks

import Garlic.Presenter

noLog :: LogFunc
noLog _ _ _ _ = return ()

runGtk :: Connection -> IO ()
runGtk connection = do
    _ <- init Nothing

    backend <- wrapConnection connection noLog
    compile (runReaderT presenter backend) >>= actuate

garlic :: IO ()
garlic = do
    connection <- open "/tmp/garlic.db"
    runGtk connection
        `catch` (\(e :: GError) -> gerrorMessage e >>= putStrLn . unpack)
        `finally` close connection
