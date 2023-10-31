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

newtype VengeanceAwaits = VengeanceAwaits AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

vengeanceAwaits :: AgendaCard VengeanceAwaits
vengeanceAwaits =
  agenda (3, A) VengeanceAwaits Cards.vengeanceAwaits (Static 5)

instance HasAbilities VengeanceAwaits where
  getAbilities (VengeanceAwaits a)
    | onSide A a =
        [ forcedAbility a 1
            $ AgendaAdvances #when
            $ AgendaWithId (toId a)
        ]
  getAbilities (VengeanceAwaits a) =
    [ mkAbility a 2
        $ Objective
        $ ForcedAbility
        $ EnemyDefeated #after Anyone ByAny
        $ enemyIs Enemies.umordhoth
    ]

instance RunMessage VengeanceAwaits where
  runMessage msg a@(VengeanceAwaits attrs@AgendaAttrs {..}) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      actIds <- selectList AnyAct
      umordhoth <- getSetAsideCard Enemies.umordhoth
      if "01146" `elem` actIds
        then do
          (ritualSite, placeRitualSite) <- placeSetAsideLocation Locations.ritualSite
          createUmordhoth <- createEnemyAt_ umordhoth ritualSite Nothing
          pushAll [placeRitualSite, createUmordhoth]
        else do
          ritualSite <- getJustLocationByName "Ritual Site"
          enemies <- selectTargets $ enemyAt ritualSite
          createUmordhoth <- createEnemyAt_ umordhoth ritualSite Nothing
          pushAll $ map (toDiscard attrs) enemies <> [createUmordhoth]
      pure a
    UseCardAbility _ (isSource attrs -> True) 2 _ _ -> do
      push R2
      pure a
    AdvanceAgenda aid | aid == agendaId && onSide B attrs -> do
      actIds <- selectList AnyAct
      pushAll $ map (toDiscard GameSource) actIds
      pure a
    _ -> VengeanceAwaits <$> runMessage msg attrs
