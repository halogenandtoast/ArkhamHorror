module Arkham.Treachery.Cards.MeddlesomeFamiliar (
  meddlesomeFamiliar,
  MeddlesomeFamiliar (..),
) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Enemy
import Arkham.Helpers.Investigator
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
    Revelation iid source | isSource attrs source -> do
      inPlay <- getEnemyIsInPlay Enemies.brownJenkin
      push $
        FindEncounterCard
          iid
          (toTarget attrs)
          [FromEncounterDeck, FromEncounterDiscard]
          ( cardIs $
              if inPlay
                then Enemies.swarmOfRats
                else Enemies.brownJenkin
          )
      pure t
    FoundEncounterCard iid (isTarget attrs -> True) (toCard -> card) | card `cardMatch` cardIs Enemies.brownJenkin -> do
      location <- getJustLocation iid
      spawnBrownJenkin <- createEnemyAt_ card location Nothing
      pushAll [spawnBrownJenkin, InvestigatorAssignDamage iid (toSource attrs) DamageAny 1 0]
      pure t
    FoundEncounterCard iid (isTarget attrs -> True) (toCard -> card) | card `cardMatch` cardIs Enemies.swarmOfRats -> do
      location <- getJustLocation iid
      pushAll [SpawnEnemyAtEngagedWith card location iid]
      pure t
    _ -> MeddlesomeFamiliar <$> runMessage msg attrs
