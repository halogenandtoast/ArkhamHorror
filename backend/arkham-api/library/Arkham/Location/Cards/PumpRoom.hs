module Arkham.Location.Cards.PumpRoom (pumpRoom, PumpRoom (..)) where

import Arkham.Ability
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype PumpRoom = PumpRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pumpRoom :: LocationCard PumpRoom
pumpRoom = location PumpRoom Cards.pumpRoom 0 (Static 0)

instance HasAbilities PumpRoom where
  getAbilities (PumpRoom attrs) =
    extendRevealed1 attrs $ skillTestAbility $ restricted attrs 1 Here actionAbility

instance RunMessage PumpRoom where
  runMessage msg l@(PumpRoom attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseOneM iid do
        for_ [#intellect, #agility] \sType ->
          skillLabeled sType $ beginSkillTest sid iid (attrs.ability 1) iid sType (Fixed 2)
      pure l
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n -> do
      floodedLocations <- select FloodedLocation
      chooseOneM iid do
        questionLabeled "Choose location to decrease flood level"
        targets floodedLocations \lid -> do
          decreaseThisFloodLevel lid
          when (n < 2) do
            floodable <- select CanHaveFloodLevelIncreased
            chooseOneM iid do
              questionLabeled "Choose location to increase flood level"
              targets (deleteFirst lid floodable) increaseThisFloodLevel

      pure l
    _ -> PumpRoom <$> liftRunMessage msg attrs
