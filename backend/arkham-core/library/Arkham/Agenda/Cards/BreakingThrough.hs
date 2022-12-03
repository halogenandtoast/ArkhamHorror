module Arkham.Agenda.Cards.BreakingThrough
  ( BreakingThrough(..)
  , breakingThrough
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Agenda.Types
import Arkham.Agenda.Runner
import Arkham.Card
import Arkham.Card.EncounterCard
import Arkham.Classes
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing

newtype BreakingThrough = BreakingThrough AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
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

instance RunMessage BreakingThrough where
  runMessage msg a@(BreakingThrough attrs@AgendaAttrs {..}) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source ->
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
