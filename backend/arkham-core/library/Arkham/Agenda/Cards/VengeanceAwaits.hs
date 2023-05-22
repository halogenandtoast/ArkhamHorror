module Arkham.Agenda.Cards.VengeanceAwaits (
  VengeanceAwaits (..),
  vengeanceAwaits,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Helpers
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message hiding (EnemyDefeated)
import Arkham.Resolution
import Arkham.Timing qualified as Timing

newtype VengeanceAwaits = VengeanceAwaits AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

vengeanceAwaits :: AgendaCard VengeanceAwaits
vengeanceAwaits =
  agenda (3, A) VengeanceAwaits Cards.vengeanceAwaits (Static 5)

instance HasAbilities VengeanceAwaits where
  getAbilities (VengeanceAwaits a) =
    if onSide A a
      then
        [ mkAbility a 1 $
            ForcedAbility $
              AgendaAdvances Timing.When $
                AgendaWithId $
                  toId a
        ]
      else
        [ mkAbility a 2 $
            Objective $
              ForcedAbility $
                EnemyDefeated Timing.After Anyone ByAny $
                  enemyIs Enemies.umordhoth
        ]

instance RunMessage VengeanceAwaits where
  runMessage msg a@(VengeanceAwaits attrs@AgendaAttrs {..}) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      actIds <- selectList AnyAct
      umordhoth <- getSetAsideCard Enemies.umordhoth
      if "01146" `elem` actIds
        then do
          (ritualSiteId, placeRitualSite) <-
            placeSetAsideLocation
              Locations.ritualSite
          createUmordhoth <- createEnemyAt_ umordhoth ritualSiteId Nothing
          pushAll [placeRitualSite, createUmordhoth]
        else do
          ritualSiteId <- getJustLocationIdByName "Ritual Site"
          enemies <- selectTargets $ EnemyAt $ LocationWithId ritualSiteId
          createUmordhoth <- createEnemyAt_ umordhoth ritualSiteId Nothing
          pushAll $
            [Discard (toSource attrs) enemy | enemy <- enemies]
              <> [createUmordhoth]
      pure a
    UseCardAbility _ source 2 _ _
      | isSource attrs source ->
          a <$ push (ScenarioResolution $ Resolution 2)
    AdvanceAgenda aid | aid == agendaId && onSide B attrs -> do
      actIds <- selectList AnyAct
      a <$ pushAll [Discard GameSource (ActTarget actId) | actId <- actIds]
    _ -> VengeanceAwaits <$> runMessage msg attrs
