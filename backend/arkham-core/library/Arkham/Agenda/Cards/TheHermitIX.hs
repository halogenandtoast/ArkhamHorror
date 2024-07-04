module Arkham.Agenda.Cards.TheHermitIX (TheHermitIX (..), theHermitIX) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted hiding (EnemyDefeated)
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Enemy
import Arkham.Helpers.Investigator
import Arkham.Helpers.Modifiers (ModifierType (..), modified)
import Arkham.Helpers.Query (getLead, getPlayerCount)
import Arkham.Matcher
import Arkham.Trait (Trait (WitchHouse))

newtype Metadata = Metadata {foundCardCount :: Int}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype TheHermitIX = TheHermitIX (AgendaAttrs `With` Metadata)
  deriving anyclass (IsAgenda)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theHermitIX :: AgendaCard TheHermitIX
theHermitIX = agenda (1, A) (TheHermitIX . (`with` Metadata 0)) Cards.theHermitIX (Static 4)

instance HasModifiersFor TheHermitIX where
  getModifiersFor (EnemyTarget eid) (TheHermitIX (a `With` _)) = do
    valid <- eid <=~> NonWeaknessEnemy
    modified a [HealthModifier 1 | valid]
  getModifiersFor _ _ = pure []

instance HasAbilities TheHermitIX where
  getAbilities (TheHermitIX (a `With` _)) =
    [ mkAbility a 1
        $ freeReaction
        $ EnemyDefeated #after You ByAny (mapOneOf enemyIs [Enemies.nahab, Enemies.brownJenkin])
    ]

instance RunMessage TheHermitIX where
  runMessage msg a@(TheHermitIX (attrs `With` meta)) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      lead <- getLead
      inPlay <- getEnemyIsInPlay Enemies.brownJenkin
      findEncounterCard lead attrs $ if inPlay then Enemies.swarmOfRats else Enemies.brownJenkin
      playerCount <- getPlayerCount
      when (playerCount >= 3) $ findEncounterCard lead attrs Enemies.swarmOfRats
      advanceAgendaDeck attrs
      pure a
    FoundEncounterCard lead (isTarget attrs -> True) (toCard -> card) | card `cardMatch` cardIs Enemies.brownJenkin -> do
      location <- getJustLocation lead
      createEnemyAt_ card location
      pure . TheHermitIX $ attrs `with` Metadata 1
    FoundEncounterCard _ (isTarget attrs -> True) (toCard -> card) | card `cardMatch` cardIs Enemies.swarmOfRats -> do
      let matcher
            | foundCardCount meta == 0 = LocationWithEnemy $ enemyIs Enemies.brownJenkin
            | otherwise = LocationWithTrait WitchHouse
      createEnemyAtLocationMatching_ card matcher
      pure . TheHermitIX $ attrs `With` Metadata (foundCardCount meta + 1)
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      playerCount <- getPlayerCount
      push $ GainClues iid (attrs.ability 1) $ if playerCount >= 3 then 2 else 1
      pure a
    _ -> TheHermitIX . (`with` meta) <$> liftRunMessage msg attrs
