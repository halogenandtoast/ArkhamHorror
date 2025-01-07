module Arkham.Agenda.Cards.TheBoundaryBroken (theBoundaryBroken) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.TheForgottenAge.Key
import Arkham.Card
import Arkham.Message.Lifted.Choose
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query (getLead)
import Arkham.Helpers.Log (getRecordCount, whenHasRecord)
import Arkham.Matcher

newtype TheBoundaryBroken = TheBoundaryBroken AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBoundaryBroken :: AgendaCard TheBoundaryBroken
theBoundaryBroken =
  agenda (1, A) TheBoundaryBroken Cards.theBoundaryBroken (Static 8)

instance RunMessage TheBoundaryBroken where
  runMessage msg a@(TheBoundaryBroken attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      whenHasRecord TheHarbingerIsStillAlive do
        harbinger <- genCard Enemies.harbingerOfValusia
        lead <- getLead
        yigsFury <- getRecordCount YigsFury
        locations <-
          select
            $ if yigsFury >= 6
              then locationWithInvestigator lead
              else FarthestLocationFromAll Anywhere
        chooseOrRunOneM lead $ targets locations \location -> do
          createEnemyWithAfter_ harbinger location \harbingerId -> do
            startingDamage <- getRecordCount TheHarbingerIsStillAlive
            when (startingDamage > 0) $ placeTokens attrs harbingerId #damage startingDamage
      advanceAgendaDeck attrs
      pure a
    _ -> TheBoundaryBroken <$> liftRunMessage msg attrs
