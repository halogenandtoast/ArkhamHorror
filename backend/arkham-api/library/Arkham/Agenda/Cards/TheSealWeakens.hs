module Arkham.Agenda.Cards.TheSealWeakens (theSealWeakens) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.EdgeOfTheEarth.Key
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Log
import Arkham.Helpers.Query (getLead)
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Strategy
import Arkham.Message.Lifted.Choose

newtype TheSealWeakens = TheSealWeakens AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSealWeakens :: AgendaCard TheSealWeakens
theSealWeakens = agenda (1, A) TheSealWeakens Cards.theSealWeakens (Static 3)

instance RunMessage TheSealWeakens where
  runMessage msg a@(TheSealWeakens attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      xs <- select $ NearestLocationToAny "Mist-Pylon"
      lead <- getLead
      chooseTargetM lead xs $ createSetAsideEnemy_ Enemies.theNamelessMadness
      whenHasRecord TheTruthOfTheMirageEludesYou do
        eachInvestigator (`forInvestigator` msg)

      advanceAgendaDeck attrs
      pure a
    ForInvestigator iid (AdvanceAgenda (isSide B attrs -> True)) -> do
      search iid attrs iid [fromDeck] (basic "Tekeli-li") (defer attrs IsNotDraw)
      pure a
    SearchFound iid (isTarget attrs -> True) _ cards | notNull cards -> do
      for_ cards obtainCard 
      focusCards cards do
        chooseOneAtATimeM iid do
          targets cards \c -> do
            cardResolutionModifier c attrs c LeaveCardWhereItIs
            drawCard iid c

      shuffleCardsIntoDeck iid cards

      pure a
    _ -> TheSealWeakens <$> liftRunMessage msg attrs
