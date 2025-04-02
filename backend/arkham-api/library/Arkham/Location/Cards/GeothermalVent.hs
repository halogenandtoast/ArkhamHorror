module Arkham.Location.Cards.GeothermalVent (geothermalVent) where

import Arkham.Campaigns.EdgeOfTheEarth.Seal
import Arkham.Helpers.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Ability
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TheHeartOfMadness.Helpers

newtype GeothermalVent = GeothermalVent LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

geothermalVent :: LocationCard GeothermalVent
geothermalVent = location GeothermalVent Cards.geothermalVent 4 (PerPlayer 1)

instance HasAbilities GeothermalVent where
  getAbilities (GeothermalVent a) =
    extendRevealed1 a
      $ skillTestAbility
      $ restricted a 1 (youExist (InvestigatorWithDormantSeal SealD)) actionAbility

instance RunMessage GeothermalVent where
  runMessage msg l@(GeothermalVent attrs) = runQueueT $ case msg of
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
          removeChaosToken #frost
        labeled "Do not spend clues" nothing

      pure l
    _ -> GeothermalVent <$> liftRunMessage msg attrs
