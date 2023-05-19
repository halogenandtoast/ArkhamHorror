module Arkham.Agenda.Cards.TheFamiliar (
  TheFamiliar (..),
  theFamiliar,
) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Helpers.Act
import Arkham.Helpers.Card
import Arkham.Helpers.Enemy
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message hiding (EnemyDefeated)
import Arkham.Prelude
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Cards qualified as Treacheries

newtype TheFamiliar = TheFamiliar AgendaAttrs
  deriving anyclass (IsAgenda)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theFamiliar :: AgendaCard TheFamiliar
theFamiliar = agenda (2, A) TheFamiliar Cards.theFamiliar (Static 6)

instance HasModifiersFor TheFamiliar where
  getModifiersFor (EnemyTarget eid) (TheFamiliar a) = do
    isNonWeakness <- eid <=~> NonWeaknessEnemy
    pure $ toModifiers a [HealthModifier 2 | isNonWeakness]
  getModifiersFor _ _ = pure []

instance HasAbilities TheFamiliar where
  getAbilities (TheFamiliar a) =
    [ mkAbility a 1 $
        ReactionAbility
          ( EnemyDefeated Timing.After You ByAny $
              EnemyOneOf [enemyIs Enemies.nahab, enemyIs Enemies.brownJenkin]
          )
          Free
    ]

instance RunMessage TheFamiliar where
  runMessage msg a@(TheFamiliar attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      step <- getCurrentActStep
      stepMsg <- case step of
        1 -> do
          nahab <- getSetAsideCard Enemies.nahab
          walterGilmansRoom <- selectJust $ locationIs Locations.walterGilmansRoom
          createEnemyAt_ nahab walterGilmansRoom Nothing
        2 -> do
          nahab <- getSetAsideCard Enemies.nahab
          keziahsRoom <- selectJust $ locationIs Locations.keziahsRoom
          createEnemyAt_ nahab keziahsRoom Nothing
        3 -> do
          nahab <- getUniqueEnemy Enemies.nahab
          pure $ PlaceDoom (toTarget nahab) 1
        _ -> error "Invalid act step"
      ghostlyPresences <-
        getSetAsideCardsMatching $ cardIs Treacheries.ghostlyPresence
      pushAll
        [ stepMsg
        , ShuffleCardsIntoDeck Deck.EncounterDeck ghostlyPresences
        , NextAdvanceAgendaStep aid 1
        , advanceAgendaDeck attrs
        ]
      pure a
    NextAdvanceAgendaStep aid 1 | aid == toId attrs -> do
      location <- selectJust $ LocationWithEnemy $ enemyIs Enemies.nahab
      brownJenkin <- findUniqueCard Enemies.brownJenkin
      pushM $ createEnemyAt_ brownJenkin location Nothing
      pure a
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      playerCount <- getPlayerCount
      push $ GainClues iid $ if playerCount >= 3 then 2 else 1
      pure a
    _ -> TheFamiliar <$> runMessage msg attrs
