module Arkham.Agenda.Cards.BreakingThrough (
  BreakingThrough (..),
  breakingThrough,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Timing qualified as Timing

newtype BreakingThrough = BreakingThrough AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

breakingThrough :: AgendaCard BreakingThrough
breakingThrough =
  agenda (3, A) BreakingThrough Cards.breakingThrough (Static 6)

instance HasAbilities BreakingThrough where
  getAbilities (BreakingThrough x) =
    [ mkAbility x 1
        $ ForcedAbility
        $ MovedBy
          Timing.After
          You
          Matcher.EncounterCardSource
    ]

instance RunMessage BreakingThrough where
  runMessage msg a@(BreakingThrough attrs@AgendaAttrs {..}) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $ InvestigatorAssignDamage iid source DamageAny 0 1
      pure a
    AdvanceAgenda aid | aid == agendaId && onSide B attrs -> do
      yogSothothSpawnLocation <-
        fromMaybeM
          (getJustLocationByName "Another Dimension")
          (getLocationByName "The Edge of the Universe")
      yogSothoth <- getSetAsideCard Enemies.yogSothoth
      createYogSothoth <-
        createEnemyAt_
          yogSothoth
          yogSothothSpawnLocation
          Nothing
      pushAll [createYogSothoth, advanceAgendaDeck attrs]
      pure a
    _ -> BreakingThrough <$> runMessage msg attrs
