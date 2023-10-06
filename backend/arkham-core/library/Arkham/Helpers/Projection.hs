module Arkham.Helpers.Projection where

import Arkham.Prelude

import Arkham.Classes.Entity
import Arkham.Classes.HasGame
import Arkham.Field
import Arkham.Projection

withMaxField :: (HasGame m, Projection a, b ~ EntityId a) => Field a Int -> [b] -> m [b]
withMaxField f ids = do
  fieldPairs <- forToSnd ids (field f)
  let maxVal = getMax0 $ foldMap (Max0 . snd) fieldPairs
  pure $ map fst $ filter ((== maxVal) . snd) fieldPairs
