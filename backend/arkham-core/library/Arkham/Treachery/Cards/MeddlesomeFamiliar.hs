module Arkham.Treachery.Cards.MeddlesomeFamiliar (
  meddlesomeFamiliar,
  MeddlesomeFamiliar (..),
) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Creation
import Arkham.Helpers.Enemy
import Arkham.Matcher
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner
import Arkham.Zone

newtype MeddlesomeFamiliar = MeddlesomeFamiliar TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

meddlesomeFamiliar :: TreacheryCard MeddlesomeFamiliar
meddlesomeFamiliar = treachery MeddlesomeFamiliar Cards.meddlesomeFamiliar

instance RunMessage MeddlesomeFamiliar where
  runMessage msg t@(MeddlesomeFamiliar attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      brownJenkinInPlay <- getEnemyIsInPlay Enemies.brownJenkin
      push $
        findEncounterCard iid attrs [FromEncounterDeck, FromEncounterDiscard] $
          if brownJenkinInPlay then Enemies.swarmOfRats else Enemies.brownJenkin
      pure t
    FoundEncounterCard iid (isTarget attrs -> True) (toCard -> card) | card `cardMatch` Enemies.brownJenkin -> do
      spawnBrownJenkin <- createEnemy2 card (locationWithInvestigator iid)
      pushAll [CreateEnemy spawnBrownJenkin, InvestigatorAssignDamage iid (toSource attrs) DamageAny 1 0]
      pure t
    FoundEncounterCard iid (isTarget attrs -> True) (toCard -> card) | card `cardMatch` Enemies.swarmOfRats -> do
      spawnSwarmOfRats <- createEnemy2 card (locationWithInvestigator iid)
      push $
        CreateEnemy $
          spawnSwarmOfRats
            { enemyCreationAfter = [InvestigatorAssignDamage iid (toSource attrs) DamageAny 1 0]
            }
      pure t
    _ -> MeddlesomeFamiliar <$> runMessage msg attrs
