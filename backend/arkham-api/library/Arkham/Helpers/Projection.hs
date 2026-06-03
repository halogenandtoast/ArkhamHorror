module Arkham.Helpers.Projection where

import Arkham.Classes.Entity
import Arkham.Classes.HasGame
import Arkham.Field
import Arkham.Prelude
import Arkham.Projection
import Arkham.Tracing
withMaybeMaxField
  :: (HasGame m, Tracing m, Projection a, b ~ EntityId a) => Field a (Maybe Int) -> [b] -> m [b]
withMaybeMaxField f ids = do
  fieldPairs <- forToSnd ids (field f) <&> mapMaybe (\(i, mVal) -> (i,) <$> mVal)
  let maxVal = getMax0 $ foldMap (Max0 . snd) fieldPairs
  pure $ map fst $ filter ((== maxVal) . snd) fieldPairs
