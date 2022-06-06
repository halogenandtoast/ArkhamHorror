module Arkham.Agenda.Cards.RestrictedAccess
  ( RestrictedAccess(..)
  , restrictedAccess
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Agenda.Attrs
import Arkham.Agenda.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Window (Window(..))
import Arkham.Window qualified as Window

newtype RestrictedAccess = RestrictedAccess AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

restrictedAccess :: AgendaCard RestrictedAccess
restrictedAccess =
  agenda (1, A) RestrictedAccess Cards.restrictedAccess (Static 5)

instance HasAbilities RestrictedAccess where
  getAbilities (RestrictedAccess x) =
    [ mkAbility x 1 $ ForcedAbility $ EnemySpawns Timing.When Anywhere $ enemyIs
        Cards.huntingHorror
    ]

instance AgendaRunner env => RunMessage RestrictedAccess where
  runMessage msg a@(RestrictedAccess attrs) = case msg of
    UseCardAbility _ source [Window _ (Window.EnemySpawns eid _)] 1 _
      | isSource attrs source -> do
        mShadowSpawnedId <- fmap unStoryTreacheryId
          <$> getId (toCardCode Treacheries.shadowSpawned)
        a <$ case mShadowSpawnedId of
          Just tid -> push $ PlaceResources (TreacheryTarget tid) 1
          Nothing -> do
            shadowSpawned <- getSetAsideCard Treacheries.shadowSpawned
            push $ AttachStoryTreacheryTo shadowSpawned (EnemyTarget eid)
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      mHuntingHorrorId <- selectOne $ enemyIs Cards.huntingHorror
      a <$ case mHuntingHorrorId of
        Just eid -> pushAll
          [ PlaceDoom (EnemyTarget eid) 1
          , AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)
          ]
        Nothing -> push $ FindEncounterCard
          leadInvestigatorId
          (toTarget attrs)
          (CardWithCardCode "02141")
    FoundEnemyInVoid _ target eid | isTarget attrs target -> do
      lid <- fromJustNote "Museum Halls missing"
        <$> selectOne (LocationWithTitle "Museum Halls")
      a <$ pushAll
        [ EnemySpawnFromVoid Nothing lid eid
        , AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)
        ]
    FoundEncounterCard _ target ec | isTarget attrs target -> do
      lid <- fromJustNote "Museum Halls missing"
        <$> selectOne (LocationWithTitle "Museum Halls")
      a <$ pushAll
        [ SpawnEnemyAt (EncounterCard ec) lid
        , AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)
        ]
    _ -> RestrictedAccess <$> runMessage msg attrs
