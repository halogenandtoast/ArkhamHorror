module Arkham.Location.Cards.TitanicRamp_182 (titanicRamp_182) where

import Arkham.Ability
import Arkham.Capability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype TitanicRamp_182 = TitanicRamp_182 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

titanicRamp_182 :: LocationCard TitanicRamp_182
titanicRamp_182 = location TitanicRamp_182 Cards.titanicRamp_182 1 (PerPlayer 1)

instance HasModifiersFor TitanicRamp_182 where
  getModifiersFor (TitanicRamp_182 a) =
    modifySelf a [AdditionalCostToLeave $ SkillTestCost (a.ability 1) #agility (Fixed 4)]

instance HasAbilities TitanicRamp_182 where
  getAbilities (TitanicRamp_182 a) =
    extend1
      a
      $ restricted a 1 (exists $ InvestigatorAt (orConnected YourLocation) <> can.spend.clues)
      $ SilentForcedAbility
      $ InitiatedSkillTest
        #after
        You
        AnySkillType
        AnySkillTestValue
        (SkillTestSourceMatches (SourceIs (a.ability 1)))

instance RunMessage TitanicRamp_182 where
  runMessage msg l@(TitanicRamp_182 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseOneM iid do
        labeled
          "Have any investigator at your location or a connecting location may spend 1 clue to have you automatically succeed at this test."
          do
            withCost iid (GroupClueCost (Static 1) (orConnected $ locationWithInvestigator iid)) passSkillTest
        labeled "Do not spend clues" nothing
      pure l
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      cancelMovement attrs iid
      pure l
    _ -> TitanicRamp_182 <$> liftRunMessage msg attrs
