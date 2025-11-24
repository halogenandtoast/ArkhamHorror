module Arkham.Location.Cards.PumpRoom (pumpRoom) where

import Arkham.Ability
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype PumpRoom = PumpRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pumpRoom :: LocationCard PumpRoom
pumpRoom = location PumpRoom Cards.pumpRoom 3 (Static 0)

instance HasAbilities PumpRoom where
  getAbilities (PumpRoom attrs) =
    extendRevealed1 attrs $ skillTestAbility $ restricted attrs 1 Here actionAbility

instance RunMessage PumpRoom where
  runMessage msg l@(PumpRoom attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseBeginSkillTest sid iid (attrs.ability 1) iid [#intellect, #agility] (Fixed 2)
      pure l
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n -> do
      floodedLocations <- select FloodedLocation
      chooseOneM iid do
        questionLabeled "Choose location to decrease flood level"
        targets floodedLocations \lid -> do
          decreaseThisFloodLevel lid
          floodable <- select $ CanHaveFloodLevelIncreased <> not_ (be lid)
          chooseOneM iid do
            questionLabeled "Choose location to increase flood level"
            when (n >= 2) $ withI18n skip_
            targets (deleteFirst lid floodable) increaseThisFloodLevel
      pure l
    _ -> PumpRoom <$> liftRunMessage msg attrs
