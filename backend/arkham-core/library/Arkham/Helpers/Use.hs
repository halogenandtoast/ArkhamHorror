module Arkham.Helpers.Use (module Arkham.Helpers.Use, module X) where

import Arkham.Asset.Types (Field (..))
import Arkham.Asset.Uses as X
import Arkham.Classes.HasGame
import Arkham.Helpers.Calculation
import Arkham.Id
import Arkham.Prelude
import Arkham.Projection

getAssetUses :: HasGame m => UseType -> AssetId -> m Int
getAssetUses k = fieldMap AssetUses (findWithDefault 0 k)

toStartingUses :: HasGame m => Uses GameCalculation -> m (Map UseType Int)
toStartingUses = fmap toMap . asStartingUses
 where
  toMap = \case
    Uses uType value -> singletonMap uType value
    UsesWithLimit uType value _ -> singletonMap uType value
    NoUses -> mempty

asStartingUses :: HasGame m => Uses GameCalculation -> m (Uses Int)
asStartingUses (Uses uType gameValue) = Uses uType <$> calculate gameValue
asStartingUses (UsesWithLimit uType gameValue limitValue) =
  UsesWithLimit uType <$> calculate gameValue <*> calculate limitValue
asStartingUses NoUses = pure NoUses

startingUseCountFor :: HasGame m => UseType -> Uses GameCalculation -> m Int
startingUseCountFor uType = fmap (findWithDefault 0 uType) . toStartingUses

hasUsesFor :: UseType -> Uses GameCalculation -> Bool
hasUsesFor uType uses = useType uses == Just uType
