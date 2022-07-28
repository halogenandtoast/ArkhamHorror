module Arkham.Agenda.Cards.VengeanceAwaits
  ( VengeanceAwaits(..)
  , vengeanceAwaits
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Attrs
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Helpers
import Arkham.Agenda.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Id
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message hiding ( EnemyDefeated )
import Arkham.Resolution
import Arkham.Target
import Arkham.Timing qualified as Timing

newtype VengeanceAwaits = VengeanceAwaits AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

vengeanceAwaits :: AgendaCard VengeanceAwaits
vengeanceAwaits =
  agenda (3, A) VengeanceAwaits Cards.vengeanceAwaits (Static 5)

instance HasAbilities VengeanceAwaits where
  getAbilities (VengeanceAwaits a) = if onSide A a
    then
      [ mkAbility a 1
        $ ForcedAbility
        $ AgendaAdvances Timing.When
        $ AgendaWithId
        $ toId a
      ]
    else
      [ mkAbility a 2
        $ Objective
        $ ForcedAbility
        $ EnemyDefeated Timing.After Anyone
        $ enemyIs Enemies.umordhoth
      ]

instance RunMessage VengeanceAwaits where
  runMessage msg a@(VengeanceAwaits attrs@AgendaAttrs {..}) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      actIds <- selectList AnyAct
      umordhoth <- getSetAsideCard Enemies.umordhoth
      a <$ if "01146" `elem` actIds
        then do
          ritualSite <- getSetAsideCard Locations.ritualSite
          pushAll
            [ PlaceLocation ritualSite
            , CreateEnemyAt umordhoth (LocationId $ toCardId ritualSite) Nothing
            ]
        else do
          ritualSiteId <- getJustLocationIdByName "Ritual Site"
          enemies <- selectListMap EnemyTarget $ EnemyAt $ LocationWithId
            ritualSiteId
          pushAll
            $ [ Discard enemy | enemy <- enemies ]
            <> [CreateEnemyAt umordhoth ritualSiteId Nothing]
    UseCardAbility _ source _ 2 _ | isSource attrs source ->
      a <$ push (ScenarioResolution $ Resolution 2)
    AdvanceAgenda aid | aid == agendaId && onSide B attrs -> do
      actIds <- selectList AnyAct
      a <$ pushAll [ Discard (ActTarget actId) | actId <- actIds ]
    _ -> VengeanceAwaits <$> runMessage msg attrs
