{-# LANGUAGE DeriveDataTypeable #-}

module Sgf.Control.Exception
    ( FileException (..)
    , doesFileExist'
    )
  where

import Data.Typeable
import Control.Monad.IO.Class
import Control.Exception
import System.Directory


data FileException  = FileDoesNotExist FilePath
  deriving (Typeable)
instance Show FileException where
    show (FileDoesNotExist e)   = "File does not exist: '" ++ e ++ "'"
instance Exception FileException where

doesFileExist' :: MonadIO m => FilePath -> m ()
doesFileExist' x    = do
    b <- liftIO (doesFileExist x)
    if b then return ()  else throw (FileDoesNotExist x)

