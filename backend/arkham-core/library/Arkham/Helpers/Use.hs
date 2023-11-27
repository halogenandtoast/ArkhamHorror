module Arkham.Helpers.Use where

import Arkham.Prelude

import Arkham.Asset.Uses
import Arkham.Classes.HasGame
import Arkham.Game.Helpers
import Arkham.GameValue

toStartingUses :: HasGame m => Uses GameValue -> m (Map UseType Int)
toStartingUses uses = toMap <$> asStartingUses uses
 where
  toMap = \case
    Uses uType value -> singletonMap uType value
    UsesWithLimit uType value _ -> singletonMap uType value
    NoUses -> mempty

asStartingUses :: HasGame m => Uses GameValue -> m (Uses Int)
asStartingUses (Uses uType gameValue) = do
  value <- getPlayerCountValue gameValue
  pure $ Uses uType value
asStartingUses (UsesWithLimit uType gameValue limitValue) = do
  value <- getPlayerCountValue gameValue
  limit <- getPlayerCountValue limitValue
  pure $ UsesWithLimit uType value limit
asStartingUses NoUses = pure NoUses

startingUseCountFor :: HasGame m => UseType -> Uses GameValue -> m Int
startingUseCountFor uType uses = do
  u' <- toStartingUses uses
  pure $ findWithDefault 0 uType u'

hasUsesFor :: UseType -> Uses GameValue -> Bool
hasUsesFor uType uses = useType uses == Just uType
