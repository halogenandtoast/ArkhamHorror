module Arkham.Helpers.Use (module Arkham.Helpers.Use, module X) where

import Arkham.Asset.Types (Field (..))
import Arkham.Asset.Uses as X
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Helpers.Calculation
import Arkham.Helpers.Modifiers (ModifierType (AdditionalStartingUses), getCombinedModifiers)
import Arkham.Id
import Arkham.Prelude
import Arkham.Projection
import Arkham.Target

getAssetUses :: HasGame m => UseType -> AssetId -> m Int
getAssetUses k = fieldMap AssetUses (findWithDefault 0 k)

toModifiedStartingUses
  :: (HasGame m, IsCard a, Targetable a) => a -> Uses GameCalculation -> m (Map UseType Int)
toModifiedStartingUses a startingUses = do
  modifiers <- getCombinedModifiers [toTarget a, CardIdTarget (toCardId a)]
  startingUses <- toStartingUses startingUses
  foldM applyModifier startingUses modifiers
 where
  applyModifier usesMap (AdditionalStartingUses n) = case startingUses of
    Uses uType _ -> pure $ adjustMap (+ n) uType usesMap
    UsesWithLimit uType _ pl -> do
      l <- calculate pl
      pure $ adjustMap (min l . (+ n)) uType usesMap
    _ -> pure usesMap
  applyModifier m _ = pure m

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
