module Arkham.Helpers.Projection where

import Arkham.Prelude

import Arkham.Classes.Entity
import Arkham.Field
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Projection

withMaxField :: (Projection a, b ~ EntityId a) => Field a Int -> [b] -> GameT [b]
withMaxField f ids = do
  fieldPairs <- forToSnd ids (field f)
  let maxVal = getMax0 $ foldMap (Max0 . snd) fieldPairs
  pure $ map fst $ filter ((== maxVal) . snd) fieldPairs
