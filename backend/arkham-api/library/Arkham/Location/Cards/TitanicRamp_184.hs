module Arkham.Location.Cards.TitanicRamp_184 (titanicRamp_184) where

import Arkham.Ability
import Arkham.Capability
import Arkham.ForMovement
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype TitanicRamp_184 = TitanicRamp_184 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

titanicRamp_184 :: LocationCard TitanicRamp_184
titanicRamp_184 = location TitanicRamp_184 Cards.titanicRamp_184 3 (PerPlayer 1)

instance HasModifiersFor TitanicRamp_184 where
  getModifiersFor (TitanicRamp_184 a) =
    modifySelf a [AdditionalCostToLeave $ SkillTestCost (a.ability 1) #agility (Fixed 2)]

instance HasAbilities TitanicRamp_184 where
  getAbilities (TitanicRamp_184 a) =
    extend1 a
      $ restricted
        a
        1
        (exists $ InvestigatorAt (orConnected NotForMovement YourLocation) <> can.spend.clues)
      $ SilentForcedAbility
      $ InitiatedSkillTest
        #after
        You
        AnySkillType
        AnySkillTestValue
        (SkillTestSourceMatches (SourceIs (a.ability 1)))

instance RunMessage TitanicRamp_184 where
  runMessage msg l@(TitanicRamp_184 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseOneM iid do
        labeled
          "Have any investigator at your location or a connecting location may spend 1 clue to have you automatically succeed at this test."
          do
            withCost
              iid
              (GroupClueCost (Static 1) (orConnected NotForMovement $ locationWithInvestigator iid))
              passSkillTest
        labeled "Do not spend clues" nothing
      pure l
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      cancelMovement attrs iid
      pure l
    _ -> TitanicRamp_184 <$> liftRunMessage msg attrs
