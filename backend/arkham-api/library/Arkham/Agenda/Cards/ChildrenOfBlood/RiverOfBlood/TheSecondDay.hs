module Arkham.Agenda.Cards.ChildrenOfBlood.RiverOfBlood.TheSecondDay (theSecondDay) where

import Arkham.Ability
import Arkham.Agenda.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Card
import Arkham.Enemy.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Enemies
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Query (getLead, getSetAsideCardsMatching)
import Arkham.Helpers.Scenario (getEncounterDeck, scenarioFieldMap)
import Arkham.Location.Types (Field (LocationCardsUnderneath))
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Projection
import Arkham.Scenario.Types (Field (ScenarioTokens))
import Arkham.Token qualified as Token
import Arkham.Trait (Trait (Civilian, Lair))

newtype TheSecondDay = TheSecondDay AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSecondDay :: AgendaCard TheSecondDay
theSecondDay = agenda (3, A) TheSecondDay Cards.theSecondDay (Static 4)

instance HasAbilities TheSecondDay where
  getAbilities (TheSecondDay a) =
    [ restricted
        a
        1
        (exists $ EnemyWithTitle "Julia Stern" <> oneOf [ReadyEnemy, EnemyAt (LocationWithTrait Lair)])
        $ forced
        $ PhaseEnds #when #enemy
    ]

instance RunMessage TheSecondDay where
  runMessage msg a@(TheSecondDay attrs) = runQueueT $ case msg of
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
      doStep 3 msg
      advanceAgendaDeck attrs
      pure a
    DoStep 1 (AdvanceAgenda (isSide B attrs -> True)) -> do
      damage <- scenarioFieldMap ScenarioTokens (Token.countTokens Token.Damage)
      withMatch (EnemyWithTitle "Julia Stern") \julia ->
        moveTokens attrs ScenarioSource julia Token.Damage damage
      pure a
    DoStep 2 (AdvanceAgenda (isSide B attrs -> True)) -> do
      whenNone (EnemyWithTrait Civilian) do
        lead <- getLead
        civilians <- getSetAsideCardsMatching (cardIs Enemies.waterfrontCivilian)
        for_ (headMay civilians) $ drawCard lead
      pure a
    DoStep 3 (AdvanceAgenda (isSide B attrs -> True)) -> do
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
              traverse_ obtainCard (juliaCard : topOfEncounterDeck)
              placeUnderneath lid =<< shuffleM (juliaCard : topOfEncounterDeck)
            else whenMatch julia ReadyEnemy do
              moveTowardsMatching (attrs.ability 1) julia $ NearestLocationToLocation lid $ LocationWithTrait Lair
      pure a
    _ -> TheSecondDay <$> liftRunMessage msg attrs
