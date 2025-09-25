module Arkham.Location.Cards.TrophyRoom (trophyRoom) where

import Arkham.Ability
import Arkham.Capability
import Arkham.GameValue
import Arkham.Helpers.Cost
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.AtDeathsDoorstep.Helpers

newtype TrophyRoom = TrophyRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trophyRoom :: LocationCard TrophyRoom
trophyRoom = location TrophyRoom Cards.trophyRoom 2 (Static 0)

instance HasAbilities TrophyRoom where
  getAbilities (TrophyRoom a) =
    extendRevealed1 a
      $ playerLimit PerRound
      $ restricted
        a
        1
        (Here <> youExist (oneOf [can.gain.resources, investigatorWithSpendableResources 2]))
        actionAbility

instance RunMessage TrophyRoom where
  runMessage msg l@(TrophyRoom attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      hasTwoResources <- (>= 2) <$> getSpendableResources iid
      chooseOrRunOneM iid do
        whenM (can.gain.resources iid) do
          withI18n $ countVar 2 $ labeled' "gainResources" $ gainResources iid (attrs.ability 1) 2
        when hasTwoResources do
          scenarioI18n $ labeled' "trophyRoom.spendResources" do
            spendResources iid 2
            gainClues iid (attrs.ability 1) 1
      pure l
    _ -> TrophyRoom <$> liftRunMessage msg attrs
