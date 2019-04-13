{-# LANGUAGE OverloadedStrings #-}

module Ch26_HitCounter where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader

import qualified Data.Map as M
import qualified Data.Text.Lazy as TL

import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import System.Environment (getArgs)

import Web.Scotty.Trans

data Config =
  Config {
    counts :: IORef (M.Map Text Integer)
  , prefix :: Text
  }

type Scotty = ScottyT Text (ReaderT Config IO)
type Handler = ActionT Text (ReaderT Config IO)

app :: Scotty ()
app =
  get "/:key" $ do
    unprefixed <- param "key" :: Handler Text
    config <- lift ask
    let table = readIORef (counts config)
        mutateTable = writeIORef (counts config)
        key' = prefix config <> unprefixed
        value' = M.lookup key' <$> table
    newInteger <- liftIO $ do
      v <- value'
      t <- table
      case v of
        Just x  -> mutateTable (M.adjust succ key' t) >> return (succ x)
        Nothing -> mutateTable (M.insert key' 1 t) >> return 1
    html $
      mconcat [ "<h1>Success! Count was: "
              , TL.pack $ show newInteger
              , "</h1>"
              ]

main :: IO ()
main = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let config = Config { counts = counter
                      , prefix = TL.pack prefixArg }
      runR = ($ config) . runReaderT
  scottyT 3000 runR app
