module Arkham.Agenda.Cards.PainfulHistory (painfulHistory) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Asset.Types (Field (..))
import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Keys
import Arkham.Campaigns.TheScarletKeys.Key.Matcher
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Act (getCurrentActStep)
import Arkham.Helpers.Query (getLead)
import Arkham.Matcher hiding (AssetCard)
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenarios.ShadesOfSuffering.Helpers

newtype PainfulHistory = PainfulHistory AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

painfulHistory :: AgendaCard PainfulHistory
painfulHistory = agenda (1, A) PainfulHistory Cards.painfulHistory (Static 3)

instance RunMessage PainfulHistory where
  runMessage msg a@(PainfulHistory attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator (`loseAllClues` attrs)
      allies <- select $ AssetWithHighestPrintedCost $ #ally <> #discardable
      lead <- getLead
      leadChooseOneM $ scenarioI18n do
        labeledValidate' (notNull allies) "painfulHistory.ally" do
          sid <- getRandom
          chooseTargetM lead allies \ally -> do
            toDiscardBy lead attrs ally
            x <- fieldMap AssetCard printedCardCost ally
            doStep x msg
            beginSkillTest sid lead attrs lead #willpower (Fixed x)
        labeled' "painfulHistory.doom" do
          selectEach (enemyIs Enemies.tzuSanNiangTheLadyWithTheRedParasol) (placeDoomOn attrs 1)
      n <- getCurrentActStep
      when (n == 1) $ advanceCurrentAct attrs
      advanceAgendaDeck attrs
      pure a
    DoStep n (AdvanceAgenda (isSide B attrs -> True)) -> do
      pure $ overAttrs (setMeta n) a
    FailedThisSkillTest _iid (isSource attrs -> True) -> do
      let n = toResultDefault 0 attrs.meta
      selectForMaybeM (scarletKeyIs Keys.theShadeReaper) $ placeTokensOn attrs #charge n
      pure a
    _ -> PainfulHistory <$> liftRunMessage msg attrs
