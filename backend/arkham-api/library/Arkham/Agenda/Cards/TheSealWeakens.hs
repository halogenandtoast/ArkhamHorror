module Arkham.Agenda.Cards.TheSealWeakens (theSealWeakens) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.EdgeOfTheEarth.Key
import Arkham.Card
import Arkham.Deck
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Log
import Arkham.Helpers.Query (getLead)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Scenario.Deck
import Arkham.Strategy

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
    SearchFound iid target@(isTarget attrs -> True) deck cards | notNull cards -> do
      for_ cards obtainCard
      cards' <- shuffle cards
      doStep 1 $ SearchFound iid target deck cards'
      doStep 2 msg
      pure $ TheSealWeakens $ attrs & setMeta cards
    DoStep 1 (SearchFound iid target@(isTarget attrs -> True) deck (card : cards)) -> do
      focusCards (card : cards) do
        chooseOneM iid $ targeting card do
          cardResolutionModifier card attrs card LeaveCardWhereItIs
          drawCardFrom iid card deck
      doStep 1 $ SearchFound iid target deck cards
      pure a
    DoStep 2 (SearchFound _iid (isTarget attrs -> True) deck _) -> do
      let cards :: [Card] = toResultDefault [] attrs.meta
      shuffleCardsIntoDeck deck cards
      pure a
    PutCardOnBottomOfDeck _ (ScenarioDeckByKey TekeliliDeck) card -> do
      -- because cards can be returned to the deck by Miasmic Crystal we
      -- basically store which cards should be shuffled back in in the meta and
      -- then remove when it gets moved so only the remaining gets shuffled in
      let cards :: [Card] = toResultDefault [] attrs.meta
      pure $ TheSealWeakens $ attrs & setMeta (filter (/= card) cards)
    _ -> TheSealWeakens <$> liftRunMessage msg attrs
