{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : Arkham.Ai.Model
Description : The distilled learned linear ranker for the AI investigator.

A STANDARDIZED linear model, distilled offline from @ml/model/report.json@'s
@linear_*@ fields into the embedded @data/ai-model.json@ and baked into the
binary at compile time (mirroring how 'Arkham.Ai.Tags' embeds @ai-tags.json@).

The score of a choice is

@
score(choice) = sum over features f of ((value_f - mu_f) \/ sd_f) * coef_f
@

where @value_f@ comes from 'Arkham.Ai.Decision.choiceFeatures' (the very same
extractor the heuristic and the Python trainer use, so training and inference
see identical features). The actual scoring fold lives in
'Arkham.Ai.Decision.learnedScore' (it needs @choiceFeatures@); this module only
defines the model type, the embedded loader, and the runtime gate.

== Default-off guarantee

The generic ranking only switches to the learned scorer when BOTH:

  * the @ARKHAM_AI_USE_MODEL@ environment variable is truthy (read ONCE), and
  * a NON-EMPTY model is embedded (the placeholder @{coef:{},mu:{},sd:{}}@ has
    no coefficients, so it is treated as absent).

With either condition unmet, 'activeLinearModel' is 'Nothing' and the engine
keeps the hand-tuned heuristic ranking byte-for-byte. Dropping a trained
@data/ai-model.json@ in (see @ml/export_model.py@) and rebuilding with the flag
on activates it; nothing else changes.
-}
module Arkham.Ai.Model (
  LinearModel (..),
  learnedModel,
  isUsableModel,
  useModelEnabled,
  activeLinearModel,
) where

import Arkham.Prelude hiding (toLower)

import Data.Char (toLower)
import Data.FileEmbed (embedFileIfExists)
import Data.Map.Strict qualified as Map
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)

{- | A standardized linear ranking model: per-feature coefficient, mean, and
standard deviation. All three are sparse maps keyed by the 'choiceFeatures'
feature name; a feature missing from @lmCoef@ contributes nothing, a feature
missing from @lmMu@ \/ @lmSd@ standardizes against @0@ \/ @1@. Decoded from a
file shaped @{ "coef": {..}, "mu": {..}, "sd": {..} }@.
-}
data LinearModel = LinearModel
  { lmCoef :: Map Text Double
  , lmMu :: Map Text Double
  , lmSd :: Map Text Double
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON LinearModel where
  parseJSON = withObject "LinearModel" \o -> do
    lmCoef <- o .:? "coef" .!= mempty
    lmMu <- o .:? "mu" .!= mempty
    lmSd <- o .:? "sd" .!= mempty
    pure LinearModel {..}

instance ToJSON LinearModel where
  toJSON m = object ["coef" .= lmCoef m, "mu" .= lmMu m, "sd" .= lmSd m]

{- | The decoded model embedded at compile time from @data/ai-model.json@
(resolved relative to the @backend/arkham-api@ package directory), or 'Nothing'
when no such file exists. A present-but-malformed file fails the build loudly,
exactly like @ai-tags.json@.

The build ships a placeholder empty model so this is normally @Just@ an
all-zero 'LinearModel'; 'isUsableModel' \/ 'activeLinearModel' then treat that
as absent so the heuristic stays in charge until a real model is distilled in.
-}
learnedModel :: Maybe LinearModel
learnedModel =
  either (error . ("ai-model.json failed to parse: " <>)) id
    . eitherDecodeStrict
    <$> $(embedFileIfExists "data/ai-model.json")

{- | Whether a model carries any coefficients. An empty model (the embedded
placeholder, or a real model whose coefficients all dropped out) scores every
choice @0@, so we treat it as "no model" and fall back to the heuristic.
-}
isUsableModel :: LinearModel -> Bool
isUsableModel = not . Map.null . lmCoef

{- | Whether the learned ranker is requested via the @ARKHAM_AI_USE_MODEL@
environment variable, read ONCE at first use (a @NOINLINE@ CAF, the same idiom
as 'Arkham.Metrics.globalMetricsRef'). Truthy values are @1@\/@true@\/@yes@\/@on@
(case-insensitive); anything else (including unset) is off. Default off.
-}
useModelEnabled :: Bool
useModelEnabled =
  unsafePerformIO $ do
    mv <- lookupEnv "ARKHAM_AI_USE_MODEL"
    pure $ case fmap (fmap toLower) mv of
      Just v -> v `elem` ["1", "true", "yes", "on"]
      Nothing -> False
{-# NOINLINE useModelEnabled #-}

{- | The model the generic ranking should actually use, or 'Nothing' to keep the
hand-tuned heuristic. 'Just' only when the @ARKHAM_AI_USE_MODEL@ flag is on AND
a non-empty model is embedded; this is the single gate the decision engine
consults, so the default build (flag off, or only the empty placeholder
embedded) is a no-op.
-}
activeLinearModel :: Maybe LinearModel
activeLinearModel
  | useModelEnabled = case learnedModel of
      Just m | isUsableModel m -> Just m
      _ -> Nothing
  | otherwise = Nothing
