{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}

module Main where

import Control.Concurrent
import Control.Exception
import Data.List
import Control.Monad
import Control.Monad.IO.Class
import System.Metrics
import Control.Monad.Trans.State.Strict

import System.Remote.Monitoring.Statsd
import qualified System.Metrics.Counter as Counter
import qualified System.Metrics.Gauge   as Gauge

#ifdef WITH_SERVER
import System.Remote.Monitoring.Wai
#endif

-- Init the `Store` and additionally runs the server.
-- The `Store` doesn't come with any predefined metrics, for
-- ease of debug and use.
initStore :: IO (Store, Maybe ThreadId)
#ifdef WITH_SERVER
initStore = do
  store <- newStore
  svr <- forkServerWith store "localhost" 4444
  putStrLn "Server listening on http://localhost:4444 ..."
  return (serverMetricStore svr, Just $ serverThreadId svr)
#else
initStore = (,Nothing) <$> newStore
#endif

data Latch = Up | Down

flipLatch :: Latch -> Latch
flipLatch Up   = Down
flipLatch Down = Up

sampleLatch :: Latch -> Gauge.Gauge -> IO ()
sampleLatch Up   g = Gauge.add g 5
sampleLatch Down g = Gauge.subtract g 10

data AppState = AppState {
    counter :: Counter.Counter
  , latch :: Gauge.Gauge
  , latchState :: Latch
  , storage :: Store
  }

type App = StateT AppState IO

newAppState :: Store -> IO AppState
newAppState st = do
  AppState <$> createCounter "monotonically_increasing_counter" st
           <*> createGauge   "latch_gauge" st
           <*> pure Down
           <*> pure st

main :: IO ()
main = do
  (store, _) <- initStore
  appState <- newAppState store
  flip evalStateT appState $ do
    forever loop

loop :: StateT AppState IO a
loop = do
  st@AppState{..} <- get
  -- Update the metrics
  let latchState' = flipLatch latchState
  liftIO $ do
    Counter.inc counter
    sampleLatch latchState' latch
    -- Push to statsd
    -- Wait 1 second, start again.
    threadDelay 1000000
  put st { latchState = latchState' }
  loop

{-
    handle <- forkServer "localhost" 8000
    counter <- getCounter "iterations" handle
    label <- getLabel "args" handle
    event <- getDistribution "runtime" handle
    Label.set label "some text string"
    let loop n = do
            t <- timed $ evaluate $ mean [1..n]
            Distribution.add event t
            threadDelay 2000
            Counter.inc counter
            loop n
    loop 1000000
-}
