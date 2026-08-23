module Arkham.Agenda.Cards.ChildrenOfBlood.BloodMoney.FeedingFrenzyV2 (feedingFrenzyV2) where

import Arkham.Agenda.CardDefs.ChildrenOfBlood.BloodMoney qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Creation (createExhausted)
import {-# SOURCE #-} Arkham.GameEnv (findAllCards)
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Helpers.Scenario (getVictoryDisplay)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Ally, Civilian, Cultist))

newtype FeedingFrenzyV2 = FeedingFrenzyV2 AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

feedingFrenzyV2 :: AgendaCard FeedingFrenzyV2
feedingFrenzyV2 = agenda (2, A) FeedingFrenzyV2 Cards.feedingFrenzyV2 (Static 3)

instance HasModifiersFor FeedingFrenzyV2 where
  getModifiersFor (FeedingFrenzyV2 a) = do
    cards <- findAllCards (`cardMatch` partygoer)
    modifyEach a cards [GainVictory 0]

instance RunMessage FeedingFrenzyV2 where
  runMessage msg a@(FeedingFrenzyV2 attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      inVictory <- filter (`cardMatch` partygoer) <$> getVictoryDisplay
      -- both the Children of Blood and Blood Money printings
      children <- getSetAsideCardsMatching (#enemy <> CardWithTitle "Child of Blood")
      let (spawning, remaining) = splitAt (length inVictory) children
      ls <- select $ LocationWithMostInvestigators Anywhere
      leadChooseOrRunOneM $ targets ls \lid ->
        for_ spawning \card -> createEnemyWith_ card lid createExhausted
      shuffleEncounterDiscardBackIn
      shuffleCardsIntoDeck Deck.EncounterDeck remaining
      advanceAgendaDeck attrs
      pure a
    _ -> FeedingFrenzyV2 <$> liftRunMessage msg attrs

-- | "Each {Ally}, {Civilian}, and {Cultist} card"
partygoer :: CardMatcher
partygoer = mapOneOf CardWithTrait [Ally, Civilian, Cultist]
