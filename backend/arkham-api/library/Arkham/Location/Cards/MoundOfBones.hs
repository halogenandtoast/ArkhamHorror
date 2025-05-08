module Arkham.Location.Cards.MoundOfBones (moundOfBones) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Direction
import Arkham.Draw.Types
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenario.Deck
import Arkham.Scenarios.ThePallidMask.Helpers

newtype MoundOfBones = MoundOfBones LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moundOfBones :: LocationCard MoundOfBones
moundOfBones =
  locationWith MoundOfBones Cards.moundOfBones 1 (PerPlayer 1)
    $ connectsToAdjacent
    . (costToEnterUnrevealedL .~ GroupClueCost (PerPlayer 1) YourLocation)

instance HasAbilities MoundOfBones where
  getAbilities (MoundOfBones a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ RevealLocation #when Anyone (be a)

instance RunMessage MoundOfBones where
  runMessage msg l@(MoundOfBones attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      n <- countM (directionEmpty attrs) [Above, RightOf, Below, LeftOf]
      push $ DrawCards iid $ targetCardDraw attrs CatacombsDeck n
      atEndOfRound attrs (do_ msg)
      pure l
    Do (UseThisAbility iid (isSource attrs -> True) 1) -> do
      findEncounterCard iid attrs $ cardIs Enemies.malformedSkeleton 
      pure l
    FoundEncounterCard _iid target card | isTarget attrs target -> do
      createEnemyAt_ card attrs
      pure l
    DrewCards _ drewCards | maybe False (isTarget attrs) drewCards.target -> do
      placeDrawnLocations attrs drewCards.cards [Above, RightOf, Below, LeftOf]
      pure l
    _ -> MoundOfBones <$> liftRunMessage msg attrs
