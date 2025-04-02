module Arkham.Location.Cards.ForsakenTemple (forsakenTemple) where

import Arkham.Campaigns.EdgeOfTheEarth.Seal
import Arkham.Helpers.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Ability
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TheHeartOfMadness.Helpers

newtype ForsakenTemple = ForsakenTemple LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forsakenTemple :: LocationCard ForsakenTemple
forsakenTemple = location ForsakenTemple Cards.forsakenTemple 3 (PerPlayer 1)

instance HasAbilities ForsakenTemple where
  getAbilities (ForsakenTemple a) =
    extendRevealed1 a
      $ skillTestAbility
      $ restricted a 1 (youExist (InvestigatorWithDormantSeal SealC)) actionAbility

instance RunMessage ForsakenTemple where
  runMessage msg l@(ForsakenTemple attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #combat (Fixed 3)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      targetAmount <- perPlayer 1
      iids <- select $ investigatorAt attrs
      chooseOneM iid do
        labeled "Spend 1 {perPlayer} clues as a group to activate the seal" do
          push $ SpendClues targetAmount iids
          activateSeal SealD
        labeled "Do not spend clues" nothing

      pure l
    _ -> ForsakenTemple <$> liftRunMessage msg attrs
