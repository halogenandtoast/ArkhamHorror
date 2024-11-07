module Arkham.Act.Cards.ArkhamAsylum (ArkhamAsylum (..), arkhamAsylum) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Query (getInvestigators)
import Arkham.Message.Lifted.Choose
import Arkham.ScenarioLogKey
import Arkham.SkillTest.Type
import Arkham.SkillType

newtype Metadata = Metadata {chosenSkills :: Set SkillType}
  deriving newtype (Show, Eq, ToJSON, FromJSON)

newtype ArkhamAsylum = ArkhamAsylum ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

arkhamAsylum :: ActCard ArkhamAsylum
arkhamAsylum =
  actWith (1, A) ArkhamAsylum Cards.arkhamAsylum (groupClueCost $ PerPlayer 3)
    $ (metaL .~ (toJSON $ Metadata mempty))

instance RunMessage ArkhamAsylum where
  runMessage msg a@(ArkhamAsylum attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      let metadata = toResult (actMeta attrs)
      let skills = setFromList [#combat, #agility, #intellect] `difference` chosenSkills metadata
      lead <- getLead
      investigators <- getInvestigators
      sid <- getRandom
      chooseOneM lead do
        for_ (setToList skills) \sk -> do
          labeled ("Any investigator tests " <> format sk) do
            chooseOrRunOneM lead do
              for_ investigators \iid -> do
                targeting iid $ beginSkillTest sid iid attrs attrs sk (Fixed 4)
        labeled "You knock her over and grab the keys" do
          remember YouTookTheKeysByForce
          advanceActDeck attrs
      pure a
    FailedSkillTest _ _ source Initiator {} (SkillSkillTest st) _ | isSource attrs source -> do
      afterSkillTest do
        push $ AdvanceAct (toId attrs) source AdvancedWithClues
      let metadata = toResult (actMeta attrs)
      pure $ ArkhamAsylum $ attrs & metaL .~ toJSON (insertSet st $ chosenSkills metadata)
    PassedSkillTest _ _ source Initiator {} _ _ | isSource attrs source -> do
      afterSkillTest $ advanceActDeck attrs
      pure a
    _ -> ArkhamAsylum <$> liftRunMessage msg attrs
