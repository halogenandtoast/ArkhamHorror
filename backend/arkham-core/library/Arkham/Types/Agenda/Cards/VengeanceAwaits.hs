module Arkham.Types.Agenda.Cards.VengeanceAwaits where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Locations
import Arkham.Types.Ability
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Helpers
import Arkham.Types.Agenda.Runner
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (EnemyDefeated)
import Arkham.Types.Resolution
import Arkham.Types.Target
import Arkham.Types.Timing qualified as Timing

newtype VengeanceAwaits = VengeanceAwaits AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor env)
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

instance AgendaRunner env => RunMessage env VengeanceAwaits where
  runMessage msg a@(VengeanceAwaits attrs@AgendaAttrs {..}) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      actIds <- getSetList @ActId ()
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
          enemies <- getSetListMap EnemyTarget ritualSiteId
          pushAll
            $ [ Discard enemy | enemy <- enemies ]
            <> [CreateEnemyAt umordhoth ritualSiteId Nothing]
    UseCardAbility _ source _ 2 _ | isSource attrs source ->
      a <$ push (ScenarioResolution $ Resolution 2)
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 3 B -> do
      actIds <- getSetList ()
      a <$ pushAll [ Discard (ActTarget actId) | actId <- actIds ]
    _ -> VengeanceAwaits <$> runMessage msg attrs
