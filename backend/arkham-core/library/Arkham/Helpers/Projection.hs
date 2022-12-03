module Arkham.Helpers.Projection where

import Arkham.Prelude

import Arkham.Field
import Arkham.Projection
import Arkham.Classes.Entity
import {-# SOURCE #-} Arkham.GameEnv

withMaxField :: (Projection a, b ~ EntityId a) => Field a Int -> [b] -> GameT [b]
withMaxField f ids = do
  fieldPairs <- forToSnd ids (field f)
  let maxVal = getMax0 $ foldMap (Max . snd) fieldPairs
  pure $ map fst $ filter ((== maxVal) . snd) fieldPairs
