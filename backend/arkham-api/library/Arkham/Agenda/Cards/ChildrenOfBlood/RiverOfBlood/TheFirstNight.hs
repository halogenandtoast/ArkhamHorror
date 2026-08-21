module Arkham.Agenda.Cards.ChildrenOfBlood.RiverOfBlood.TheFirstNight (theFirstNight) where

import Arkham.Agenda.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Card
import Arkham.Enemy.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Enemies
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.Query (getInvestigators, getSetAsideCardsMatching)
import Arkham.Helpers.Scenario (getEncounterDeck)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Trait (Trait (Lair))
import Data.List (cycle)

newtype TheFirstNight = TheFirstNight AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theFirstNight :: AgendaCard TheFirstNight
theFirstNight = agenda (2, A) TheFirstNight Cards.theFirstNight (Static 5)

instance RunMessage TheFirstNight where
  runMessage msg a@(TheFirstNight attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      withMatch (EnemyWithTitle "Julia Stern") \julia -> do
        damage <- field EnemyDamage julia
        healAllDamage attrs julia
        when (damage >= 2) $ placeTokens attrs ScenarioTarget #damage (damage `div` 2)

        lairs <- select $ LocationWithTrait Lair
        unless (null lairs) do
          topOfEncounterDeck <- map toCard . toList . take 2 <$> getEncounterDeck
          juliaCard <- flippedOverCapture julia
          cards <- shuffleM (juliaCard : topOfEncounterDeck)
          traverse_ obtainCard cards
          -- 3 cards over at most 3 lairs leaves at most one card over
          let (dealt, leftover) = splitAt (length cards `div` length lairs * length lairs) cards
          for_ (zip (cycle lairs) dealt) \(lair, card) -> placeUnderneath lair (only card)
          for_ leftover \card ->
            leadChooseOneM $ targets lairs \lair -> placeUnderneath lair (only card)
      civilians <- getSetAsideCardsMatching (cardIs Enemies.waterfrontCivilian)
      iids <- getInvestigators
      unless (null iids) $ for_ (zip (cycle iids) civilians) (uncurry drawCard)
      advanceAgendaDeck attrs
      pure a
    _ -> TheFirstNight <$> liftRunMessage msg attrs
