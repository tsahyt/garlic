{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Garlic
( 
    garlic
)
where

import Control.Exception (catch, finally)
import Control.Monad.Reader
import Control.Monad (void)
import Data.Text (unpack)
import Database.Persist.Sqlite hiding (Connection)
import Database.Sqlite
import GI.Gtk
import GI.Gio (applicationRun)
import Prelude hiding (init)
import Reactive.Banana.Frameworks

import Garlic.Presenter

noLog :: LogFunc
noLog _ _ _ _ = return ()

runGtk :: SqlBackend -> IO ()
runGtk backend = void $ do
    void $ init Nothing
    app <- applicationNew Nothing []
    compile (runReaderT (presenter app) backend) >>= actuate
    applicationRun app Nothing

garlic :: IO ()
garlic = do
    connection <- open "/tmp/garlic.db"
    backend <- wrapConnection connection noLog
    runGtk backend
        `catch` (\(e :: GError) -> gerrorMessage e >>= putStrLn . unpack)
        `finally` close' backend
