module Arkham.Metrics (
  MetricEntry (..),
  Metrics,
  newMetrics,
  enableMetrics,
  disableMetrics,
  isMetricsEnabled,
  resetMetrics,
  recordSpan,
  withMetric,
  formatMetrics,
  dumpMetricsTo,
  messageTag,
) where

import Arkham.Prelude

import Data.Data (toConstr)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GHC.Clock (getMonotonicTimeNSec)
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf (printf)

data MetricEntry = MetricEntry
  { meCount :: !Int
  , meTotalNs :: !Word64
  , meMaxNs :: !Word64
  }

emptyEntry :: MetricEntry
emptyEntry = MetricEntry 0 0 0

bumpEntry :: Word64 -> MetricEntry -> MetricEntry
bumpEntry ns e =
  MetricEntry
    { meCount = meCount e + 1
    , meTotalNs = meTotalNs e + ns
    , meMaxNs = max (meMaxNs e) ns
    }

mergeEntry :: MetricEntry -> MetricEntry -> MetricEntry
mergeEntry a b =
  MetricEntry
    { meCount = meCount a + meCount b
    , meTotalNs = meTotalNs a + meTotalNs b
    , meMaxNs = max (meMaxNs a) (meMaxNs b)
    }

type Metrics = IORef (Map Text MetricEntry)

newMetrics :: IO Metrics
newMetrics = newIORef Map.empty

-- Global, lazily-initialised registry. The default is `Nothing` (disabled),
-- so the hot path is a single IORef read with no allocation.
{-# NOINLINE globalMetricsRef #-}
globalMetricsRef :: IORef (Maybe Metrics)
globalMetricsRef = unsafePerformIO (newIORef Nothing)

enableMetrics :: IO Metrics
enableMetrics = do
  m <- newMetrics
  writeIORef globalMetricsRef (Just m)
  pure m

disableMetrics :: IO ()
disableMetrics = writeIORef globalMetricsRef Nothing

isMetricsEnabled :: IO (Maybe Metrics)
isMetricsEnabled = readIORef globalMetricsRef

-- | Reset the accumulator without disabling collection.
resetMetrics :: IO ()
resetMetrics = do
  mref <- readIORef globalMetricsRef
  for_ mref $ \ref -> writeIORef ref Map.empty

recordSpan :: Text -> Word64 -> IO ()
recordSpan name ns = do
  mref <- readIORef globalMetricsRef
  for_ mref $ \ref ->
    atomicModifyIORef' ref \m ->
      (Map.insertWith mergeEntry name (bumpEntry ns emptyEntry) m, ())

-- | Time `action` and record its duration under `name` in the global metrics
-- registry, *if* metrics are enabled. When disabled, this is a single IORef
-- read with no further overhead.
withMetric :: MonadUnliftIO m => Text -> m a -> m a
withMetric name action = do
  mref <- liftIO (readIORef globalMetricsRef)
  case mref of
    Nothing -> action
    Just _ -> do
      t0 <- liftIO getMonotonicTimeNSec
      action `finally` do
        t1 <- liftIO getMonotonicTimeNSec
        liftIO $ recordSpan name (t1 - t0)

-- | Cheap constructor name extraction for `Message`. Uses Data.Data to avoid
-- forcing all fields the way `show` would.
messageTag :: Data a => a -> Text
messageTag = T.pack . show . toConstr

-- | Render the collected metrics as a sorted table (descending by total time).
formatMetrics :: Metrics -> Int -> IO Text
formatMetrics ref topN = do
  m <- readIORef ref
  let entries =
        List.take topN
          $ List.sortOn (Down . meTotalNs . snd)
          $ Map.toList m
      totalAll = sum [meTotalNs e | (_, e) <- Map.toList m]
      header =
        T.pack
          $ printf
            "%-60s %10s %12s %12s %10s %8s\n"
            ("span" :: String)
            ("count" :: String)
            ("total_ms" :: String)
            ("max_ms" :: String)
            ("avg_us" :: String)
            ("%" :: String)
      sep = T.replicate 116 "-" <> "\n"
      row (name, e) =
        T.pack
          $ printf
            "%-60s %10d %12.2f %12.2f %10.1f %7.2f%%\n"
            (T.unpack (truncate60 name))
            (meCount e)
            (nsToMs (meTotalNs e))
            (nsToMs (meMaxNs e))
            (nsToUsAvg (meTotalNs e) (meCount e))
            (pct (meTotalNs e) totalAll)
      summary =
        T.pack
          $ printf
            "\nTotal recorded span time: %.2f ms across %d distinct spans, %d span events\n"
            (nsToMs totalAll)
            (Map.size m)
            (sum [meCount e | (_, e) <- Map.toList m])
      footnote =
        "\nNote: spans nest, so totals for outer spans include their inner spans.\n\
        \Compare counts and avg_us to spot per-call hotspots; compare total_ms to\n\
        \identify aggregate hotspots. The %% column is share of total span time.\n"
  pure $ header <> sep <> T.concat (map row entries) <> summary <> footnote

dumpMetricsTo :: Maybe FilePath -> Metrics -> Int -> IO ()
dumpMetricsTo mPath ref topN = do
  txt <- formatMetrics ref topN
  case mPath of
    Nothing -> TIO.hPutStrLn stderr txt
    Just p -> TIO.writeFile p txt

truncate60 :: Text -> Text
truncate60 t
  | T.length t <= 60 = t
  | otherwise = T.take 57 t <> "..."

nsToMs :: Word64 -> Double
nsToMs ns = fromIntegral ns / 1_000_000

nsToUsAvg :: Word64 -> Int -> Double
nsToUsAvg _ 0 = 0
nsToUsAvg ns n = fromIntegral ns / fromIntegral n / 1000

pct :: Word64 -> Word64 -> Double
pct _ 0 = 0
pct part total = 100 * fromIntegral part / fromIntegral total
