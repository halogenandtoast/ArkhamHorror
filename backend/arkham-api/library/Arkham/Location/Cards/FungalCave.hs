module Arkham.Location.Cards.FungalCave (fungalCave) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype FungalCave = FungalCave LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fungalCave :: LocationCard FungalCave
fungalCave =
  locationWith FungalCave Cards.fungalCave 4 (PerPlayer 2)
    $ connectsToAdjacent
    . (costToEnterUnrevealedL .~ GroupClueCost (PerPlayer 2) YourLocation)

instance HasAbilities FungalCave where
  getAbilities (FungalCave a) =
    extendRevealed
      a
      [ groupLimit PerRound
          $ restricted a 1 Here
          $ freeReaction
          $ DiscoverClues #after You (be a) (atLeast 1)
      , mkAbility a 2 $ forced $ RoundEnds #when
      ]

instance RunMessage FungalCave where
  runMessage msg l@(FungalCave attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      healDamage iid (attrs.ability 1) 1
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      selectEach (enemyAt attrs) \enemy ->
        healDamage enemy (attrs.ability 2) 2
      pure l
    _ -> FungalCave <$> liftRunMessage msg attrs
