module Arkham.Location.Cards.TitanicRamp_184 (titanicRamp_184) where

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

newtype TitanicRamp_184 = TitanicRamp_184 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

titanicRamp_184 :: LocationCard TitanicRamp_184
titanicRamp_184 = location TitanicRamp_184 Cards.titanicRamp_184 3 (PerPlayer 1)

instance HasModifiersFor TitanicRamp_184 where
  getModifiersFor (TitanicRamp_184 a) =
    modifySelf a [AdditionalCostToLeave $ SkillTestCost (IndexedSource 1 (LocationSource a.id)) #agility (Fixed 2)]

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
        (SkillTestSourceMatches (SourceIs (IndexedSource 1 (LocationSource a.id))))

instance RunMessage TitanicRamp_184 where
  runMessage msg l@(TitanicRamp_184 attrs) = runQueueT $ case msg of
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
    _ -> TitanicRamp_184 <$> liftRunMessage msg attrs
