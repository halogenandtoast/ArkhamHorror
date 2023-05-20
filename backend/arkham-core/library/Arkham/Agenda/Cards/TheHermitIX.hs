module Arkham.Agenda.Cards.TheHermitIX (
  TheHermitIX (..),
  theHermitIX,
) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Helpers.Enemy
import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.Message hiding (EnemyDefeated)
import Arkham.Prelude
import Arkham.Timing qualified as Timing
import Arkham.Trait (Trait (WitchHouse))

newtype Metadata = Metadata {foundCardCount :: Int}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype TheHermitIX = TheHermitIX (AgendaAttrs `With` Metadata)
  deriving anyclass (IsAgenda)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theHermitIX :: AgendaCard TheHermitIX
theHermitIX =
  agenda (1, A) (TheHermitIX . (`with` Metadata 0)) Cards.theHermitIX (Static 4)

instance HasModifiersFor TheHermitIX where
  getModifiersFor (EnemyTarget eid) (TheHermitIX (a `With` _)) = do
    isNonWeakness <- eid <=~> NonWeaknessEnemy
    pure $ toModifiers a [HealthModifier 1 | isNonWeakness]
  getModifiersFor _ _ = pure []

instance HasAbilities TheHermitIX where
  getAbilities (TheHermitIX (a `With` _)) =
    [ mkAbility a 1 $
        ReactionAbility
          ( EnemyDefeated Timing.After You ByAny $
              EnemyOneOf [enemyIs Enemies.nahab, enemyIs Enemies.brownJenkin]
          )
          Free
    ]

instance RunMessage TheHermitIX where
  runMessage msg a@(TheHermitIX (attrs `With` meta)) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      lead <- getLead
      inPlay <- getEnemyIsInPlay Enemies.brownJenkin
      playerCount <- getPlayerCount
      pushAll $
        FindEncounterCard
          lead
          (toTarget attrs)
          [FromEncounterDeck, FromEncounterDiscard]
          ( cardIs $
              if inPlay
                then Enemies.swarmOfRats
                else Enemies.brownJenkin
          )
          : [ FindEncounterCard
              lead
              (toTarget attrs)
              [FromEncounterDeck, FromEncounterDiscard]
              $ cardIs
              $ Enemies.swarmOfRats
            | playerCount >= 3
            ]
            <> [ advanceAgendaDeck attrs
               ]
      pure a
    FoundEncounterCard lead (isTarget attrs -> True) (toCard -> card) | card `cardMatch` cardIs Enemies.brownJenkin -> do
      location <- getJustLocation lead
      spawnBrownJenkin <- createEnemyAt_ card location Nothing
      push spawnBrownJenkin
      pure . TheHermitIX $ attrs `with` Metadata 1
    FoundEncounterCard _ (isTarget attrs -> True) (toCard -> card) | card `cardMatch` cardIs Enemies.swarmOfRats -> do
      let matcher
            | foundCardCount meta == 0 = LocationWithEnemy $ enemyIs Enemies.brownJenkin
            | otherwise = LocationWithTrait WitchHouse
      spawnSwarmOfRats <- createEnemyAtLocationMatching_ card matcher
      push spawnSwarmOfRats
      pure . TheHermitIX $ attrs `With` Metadata (foundCardCount meta + 1)
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      playerCount <- getPlayerCount
      push $ GainClues iid (toAbilitySource attrs 1) $ if playerCount >= 3 then 2 else 1
      pure a
    _ -> TheHermitIX . (`with` meta) <$> runMessage msg attrs
