module Arkham.Agenda.Cards.ChildrenOfBlood.BloodMoney.FeedingFrenzyV1 (feedingFrenzyV1) where

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

newtype FeedingFrenzyV1 = FeedingFrenzyV1 AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

feedingFrenzyV1 :: AgendaCard FeedingFrenzyV1
feedingFrenzyV1 = agenda (2, A) FeedingFrenzyV1 Cards.feedingFrenzyV1 (Static 2)

instance HasModifiersFor FeedingFrenzyV1 where
  getModifiersFor (FeedingFrenzyV1 a) = do
    cards <- findAllCards (`cardMatch` partygoer)
    modifyEach a cards [GainVictory 0]

instance RunMessage FeedingFrenzyV1 where
  runMessage msg a@(FeedingFrenzyV1 attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      inVictory <- filter (`cardMatch` partygoer) <$> getVictoryDisplay
      -- both the Children of Blood and Blood Money printings
      children <- getSetAsideCardsMatching (#enemy <> CardWithTitle "Child of Blood")
      let (spawning, remaining) = splitAt (length inVictory) children
      ls <- select $ LocationWithMostInvestigators Anywhere
      leadChooseOrRunOneM $ targets ls \lid ->
        for_ spawning \card -> createEnemyWith_ card lid createExhausted
      replicateM_ (length inVictory - length spawning) $ addChaosToken #blood
      shuffleEncounterDiscardBackIn
      shuffleCardsIntoDeck Deck.EncounterDeck remaining
      advanceAgendaDeck attrs
      pure a
    _ -> FeedingFrenzyV1 <$> liftRunMessage msg attrs

-- | "Each {Ally}, {Civilian}, and {Cultist} card"
partygoer :: CardMatcher
partygoer = mapOneOf CardWithTrait [Ally, Civilian, Cultist]
