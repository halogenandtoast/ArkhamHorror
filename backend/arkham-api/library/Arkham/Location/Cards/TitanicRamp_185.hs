module Arkham.Location.Cards.TitanicRamp_185 (titanicRamp_185) where

import Arkham.Ability
import Arkham.Capability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype TitanicRamp_185 = TitanicRamp_185 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

titanicRamp_185 :: LocationCard TitanicRamp_185
titanicRamp_185 = location TitanicRamp_185 Cards.titanicRamp_185 4 (PerPlayer 1)

instance HasModifiersFor TitanicRamp_185 where
  getModifiersFor (TitanicRamp_185 a) =
    modifySelf a [AdditionalCostToLeave $ SkillTestCost (a.ability 1) #agility (Fixed 1)]

instance HasAbilities TitanicRamp_185 where
  getAbilities (TitanicRamp_185 a) =
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

instance RunMessage TitanicRamp_185 where
  runMessage msg l@(TitanicRamp_185 attrs) = runQueueT $ case msg of
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
    _ -> TitanicRamp_185 <$> liftRunMessage msg attrs
