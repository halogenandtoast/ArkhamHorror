module Arkham.Act.Cards.ArkhamAsylum (arkhamAsylum) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Query (getInvestigators)
import Arkham.I18n
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey
import Arkham.Scenarios.TheUnspeakableOath.Helpers
import Arkham.SkillTest.Type
import Arkham.SkillType

newtype Metadata = Metadata {chosenSkills :: Set SkillType}
  deriving newtype (Show, Eq, ToJSON, FromJSON)

newtype ArkhamAsylum = ArkhamAsylum ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

arkhamAsylum :: ActCard ArkhamAsylum
arkhamAsylum = act (1, A) ArkhamAsylum Cards.arkhamAsylum (groupClueCost $ PerPlayer 3)

instance RunMessage ArkhamAsylum where
  runMessage msg a@(ArkhamAsylum attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      let metadata = toResultDefault (Metadata mempty) (actMeta attrs)
      let skills = setFromList [#combat, #agility, #intellect] `difference` chosenSkills metadata
      investigators <- getInvestigators
      sid <- getRandom
      leadChooseOneM do
        for_ (setToList skills) \sk -> do
          withI18n $ skillVar sk $ countVar 4 $ labeled' "anyInvestigatorTests" do
            leadChooseOrRunOneM do
              targets investigators \iid -> beginSkillTest sid iid attrs attrs sk (Fixed 4)
        scenarioI18n $ labeled' "arkhamAsylum.youKnockHerOverAndGrabTheKeys" do
          remember YouTookTheKeysByForce
          advanceActDeck attrs
      pure a
    FailedSkillTest _ _ (isSource attrs -> True) Initiator {} (SkillSkillTest st) _ -> do
      afterSkillTestQuiet $ advancedWithClues attrs
      let metadata = toResultDefault (Metadata mempty) (actMeta attrs)
      pure $ ArkhamAsylum $ attrs & metaL .~ toJSON (insertSet st $ chosenSkills metadata)
    PassedSkillTest _ _ (isSource attrs -> True) Initiator {} _ _ -> do
      afterSkillTestQuiet $ advanceActDeck attrs
      pure a
    _ -> ArkhamAsylum <$> liftRunMessage msg attrs
