module Arkham.Act.Cards.QuestForTheSableGlass (questForTheSableGlass) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Campaigns.TheScarletKeys.Key
import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Keys
import Arkham.Card
import Arkham.ChaosToken.Types
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Agenda
import Arkham.Helpers.FlavorText
import Arkham.Matcher.Card
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Placement
import Arkham.Scenarios.OnThinIce.Helpers

newtype QuestForTheSableGlass = QuestForTheSableGlass ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

questForTheSableGlass :: ActCard QuestForTheSableGlass
questForTheSableGlass =
  act
    (1, A)
    QuestForTheSableGlass
    Cards.questForTheSableGlass
    (Just $ GroupClueCost (PerPlayer 3) "Condemned Gold Mine")

instance RunMessage QuestForTheSableGlass where
  runMessage msg a@(QuestForTheSableGlass attrs) = runQueueT $ scenarioI18n $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      agenda1 <- currentAgendaStepIs (== 1)
      scope "rawDeal" do
        flavor do
          p "checkAgenda"
          ul do
            li.validate agenda1 "agenda1"
            li.validate (not agenda1) "notAgenda1"
      doStep (if agenda1 then 1 else 2) msg
      advanceActDeck attrs
      pure a
    DoStep 1 msg'@(AdvanceAct (isSide B attrs -> True) _ _) -> scope "rawDeal" do
      storyWithChooseOneM' (p "rawDeal1") do
        labeled' "option3" $ doStep 3 msg'
        labeled' "option4" $ doStep 4 msg'
      pure a
    DoStep 2 msg'@(AdvanceAct (isSide B attrs -> True) _ _) -> scope "rawDeal" do
      storyWithChooseOneM' (p "rawDeal2") do
        labeled' "option3" $ doStep 3 msg'
        labeled' "option5" $ doStep 5 msg'
      pure a
    DoStep 3 (AdvanceAct (isSide B attrs -> True) _ _) -> scope "rawDeal" do
      record TheCellMadeADealWithThorne
      swapTokens ElderThing Tablet
      flavor $ p "rawDeal3"
      lead <- getLead
      thornCard <- fetchCard Enemies.thorneTheOneWithTheRedCravat
      replaceCard thornCard.id $ flipCard thornCard
      thorne <- createAssetAt Assets.thorneConsummateProfessional (InPlayArea lead)
      createScarletKeyAt_ Keys.theSableGlass (AttachedToAsset thorne Nothing)
      push $ RemoveAllCopiesOfEncounterCardFromGame $ CardFromEncounterSet Set.CrimsonConspiracy
      pure a
    DoStep 4 (AdvanceAct (isSide B attrs -> True) _ _) -> scope "rawDeal" do
      swapTokens Tablet ElderThing
      flavor $ p "rawDeal4"
      lead <- getLead
      createScarletKeyAt_ Keys.theSableGlass (AttachedToInvestigator lead)
      createEnemyAtLocationMatching_ Enemies.thorneTheOneWithTheRedCravat "Mountain Stream"
      pure a
    DoStep 5 (AdvanceAct (isSide B attrs -> True) _ _) -> scope "rawDeal" do
      swapTokens Tablet ElderThing
      flavor $ p "rawDeal5"
      thorne <- createEnemyAtLocationMatching Enemies.thorneTheOneWithTheRedCravat "Mountain Stream"
      createScarletKeyAt_ Keys.theSableGlass (AttachedToEnemy thorne)
      pure a
    _ -> QuestForTheSableGlass <$> liftRunMessage msg attrs
