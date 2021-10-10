module Arkham.Types.Agenda.Cards.BreakingThrough
  ( BreakingThrough
  , breakingThrough
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Types.Ability
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Card
import Arkham.Types.Card.EncounterCard
import Arkham.Types.Classes
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Timing qualified as Timing

newtype BreakingThrough = BreakingThrough AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

breakingThrough :: AgendaCard BreakingThrough
breakingThrough =
  agenda (3, A) BreakingThrough Cards.breakingThrough (Static 6)

instance HasAbilities BreakingThrough where
  getAbilities (BreakingThrough x) =
    [ mkAbility x 1 $ ForcedAbility $ MovedBy
        Timing.After
        You
        EncounterCardSource
    ]

instance AgendaRunner env => RunMessage env BreakingThrough where
  runMessage msg a@(BreakingThrough attrs@AgendaAttrs {..}) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (InvestigatorAssignDamage iid source DamageAny 0 1)
    AdvanceAgenda aid | aid == agendaId && onSide B attrs -> do
      yogSothothSpawnLocation <- fromMaybeM
        (getJustLocationIdByName "Another Dimension")
        (getLocationIdByName "The Edge of the Universe")
      yogSothoth <- EncounterCard <$> genEncounterCard Enemies.yogSothoth
      a <$ pushAll
        [ CreateEnemyAt yogSothoth yogSothothSpawnLocation Nothing
        , AdvanceAgendaDeck agendaDeckId (toSource attrs)
        ]
    _ -> BreakingThrough <$> runMessage msg attrs
