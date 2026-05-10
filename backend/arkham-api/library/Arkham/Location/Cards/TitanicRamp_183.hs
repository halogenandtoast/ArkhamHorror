module Arkham.Location.Cards.TitanicRamp_183 (titanicRamp_183) where

import Arkham.Ability
import Arkham.Capability
import Arkham.ForMovement
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TheHeartOfMadness.Helpers

newtype TitanicRamp_183 = TitanicRamp_183 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

titanicRamp_183 :: LocationCard TitanicRamp_183
titanicRamp_183 = location TitanicRamp_183 Cards.titanicRamp_183 2 (PerPlayer 1)

instance HasModifiersFor TitanicRamp_183 where
  getModifiersFor (TitanicRamp_183 a) =
    modifySelf a [AdditionalCostToLeave $ SkillTestCost (IndexedSource 1 (LocationSource a.id)) #agility (Fixed 3)]

instance HasAbilities TitanicRamp_183 where
  getAbilities (TitanicRamp_183 a) =
    extend1
      a
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
        (SkillTestSourceMatches (SourceIs (IndexedSource 1 (LocationSource a.id))))

instance RunMessage TitanicRamp_183 where
  runMessage msg l@(TitanicRamp_183 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseOneM iid do
        scenarioI18n' $ scope "titanicRamp" $ labeled' "spendClueToAutoSucceed" do
          withCost
            iid
            (GroupClueCost (Static 1) (orConnected NotForMovement $ locationWithInvestigator iid))
            $ withSkillTest \sid -> skillTestAutomaticallySucceeds (attrs.ability 1) sid
        withI18n $ countVar 1 $ labeled' "doNotSpendClues" nothing
      pure l
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      cancelMovement attrs iid
      pure l
    _ -> TitanicRamp_183 <$> liftRunMessage msg attrs
