module Arkham.Location.Cards.TrapRoom (trapRoom) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.GameValue
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype TrapRoom = TrapRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trapRoom :: LocationCard TrapRoom
trapRoom = location TrapRoom Cards.trapRoom 3 (PerPlayer 1)

instance HasAbilities TrapRoom where
  getAbilities (TrapRoom a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ RevealLocation #after You (be a)

instance RunMessage TrapRoom where
  runMessage msg l@(TrapRoom attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      playerCount <- getPlayerCount
      repeated (if playerCount >= 3 then 2 else 1) do
        findEncounterCard iid attrs $ cardIs Cards.swarmOfRats
      pure l
    FoundEncounterCard iid (isTarget attrs -> True) card -> do
      createEnemy_ card iid
      pure l
    _ -> TrapRoom <$> liftRunMessage msg attrs
