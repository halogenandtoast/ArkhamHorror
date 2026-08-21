module Arkham.Agenda.Cards.ChildrenOfBlood.RiverOfBlood.TheFirstDay (theFirstDay) where

import Arkham.Ability
import Arkham.Agenda.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Card
import Arkham.Enemy.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Enemies
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Query (getLead, getSetAsideCardsMatching)
import Arkham.Helpers.Scenario (getEncounterDeck)
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Projection
import Arkham.Trait (Trait (Civilian, Lair))

newtype TheFirstDay = TheFirstDay AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theFirstDay :: AgendaCard TheFirstDay
theFirstDay = agenda (1, A) TheFirstDay Cards.theFirstDay (Static 4)

instance HasAbilities TheFirstDay where
  getAbilities (TheFirstDay a) =
    [restricted a 1 (exists $ EnemyWithTitle "Julia Stern") $ forced $ PhaseEnds #when #enemy]

instance RunMessage TheFirstDay where
  runMessage msg a@(TheFirstDay attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      n <- selectSumWith length LocationCardsUnderneath Anywhere
      when (n >= 2) $ addChaosToken #blood
      lead <- getLead
      selectEach (LocationWithCardsUnderneath AnyCards) \lid -> do
        cards <- field LocationCardsUnderneath lid
        for_ cards \card -> focusCard card $ continue lead do
          unfocusCards
          obtainCard card
          if card `cardMatch` card_ #enemy
            then createEnemyAt_ card lid
            else addToEncounterDiscard [card]
      doStep 1 msg
      doStep 2 msg
      advanceAgendaDeck attrs
      pure a
    DoStep 1 (AdvanceAgenda (isSide B attrs -> True)) -> do
      whenNone (EnemyWithTrait Civilian) do
        lead <- getLead
        civilians <- getSetAsideCardsMatching (cardIs Enemies.waterfrontCivilian)
        for_ (headMay civilians) $ drawCard lead
      pure a
    DoStep 2 (AdvanceAgenda (isSide B attrs -> True)) -> do
      withMatch (EnemyWithTitle "Julia Stern") \julia -> do
        readyThis julia
        withLocationOf julia \lid -> do
          moveTowardsMatching attrs julia
            $ NearestLocationToLocation lid (LocationWithEnemy $ EnemyWithTrait Civilian)
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      withMatch (EnemyWithTitle "Julia Stern") \julia -> do
        withLocationOf julia \lid -> do
          atLair <- matches lid $ LocationWithTrait Lair
          if atLair
            then do
              topOfEncounterDeck <- map toCard . toList . take 1 <$> getEncounterDeck
              juliaCard <- flippedOverCapture julia
              obtainCard juliaCard
              placeUnderneath lid =<< shuffleM (juliaCard : topOfEncounterDeck)
            else do
              moveTowardsMatching (attrs.ability 1) julia $ NearestLocationToLocation lid $ LocationWithTrait Lair

      pure a
    _ -> TheFirstDay <$> liftRunMessage msg attrs
