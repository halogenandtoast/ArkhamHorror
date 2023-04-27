module Arkham.Treachery.Cards.MeddlesomeFamiliar (
  meddlesomeFamiliar,
  MeddlesomeFamiliar (..),
) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
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

-- Revelation - If Brown Jenkin is not in play, search the encounter deck and
-- discard pile for him, spawn him at your location, and take 1 damage.
-- Otherwise, search the encounter deck and discard pile for a Swarm of Rats,
-- spawn it engaged with you, then take 1 damage

instance RunMessage MeddlesomeFamiliar where
  runMessage msg t@(MeddlesomeFamiliar attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      mBrownJenken <- selectOne $ enemyIs Enemies.brownJenkin
      case mBrownJenken of
        Just _ ->
          push $
            FindEncounterCard
              iid
              (toTarget attrs)
              [FromEncounterDeck, FromEncounterDiscard]
              (cardIs Enemies.swarmOfRats)
        Nothing -> do
          pushAll
            [ FindEncounterCard
                iid
                (toTarget attrs)
                [FromEncounterDeck, FromEncounterDiscard]
                (cardIs Enemies.brownJenkin)
            , InvestigatorAssignDamage iid (toSource attrs) DamageAny 1 0
            ]
      pure t
    FoundEncounterCard iid (isTarget attrs -> True) (toCard -> card) | card `cardMatch` cardIs Enemies.brownJenkin -> do
      spawnBrownJenkin <- createEnemyAtLocationMatching_ card (locationWithInvestigator iid)
      push spawnBrownJenkin
      pure t
    FoundEncounterCard iid (isTarget attrs -> True) (toCard -> card) | card `cardMatch` cardIs Enemies.swarmOfRats -> do
      spawnSwarmOfRats <- createEnemyWithPlacement_ card (InThreatArea iid)
      push spawnSwarmOfRats
      pure t
    _ -> MeddlesomeFamiliar <$> runMessage msg attrs
