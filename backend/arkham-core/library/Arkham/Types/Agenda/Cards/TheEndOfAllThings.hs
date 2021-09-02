module Arkham.Types.Agenda.Cards.TheEndOfAllThings
  ( TheEndOfAllThings
  , theEndOfAllThings
  ) where

import Arkham.Prelude

import qualified Arkham.Agenda.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (EnemyDefeated)
import Arkham.Types.Resolution
import qualified Arkham.Types.Timing as Timing

newtype TheEndOfAllThings = TheEndOfAllThings AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theEndOfAllThings :: AgendaCard TheEndOfAllThings
theEndOfAllThings =
  agenda (4, A) TheEndOfAllThings Cards.theEndOfAllThings (Static 2)

instance HasAbilities TheEndOfAllThings where
  getAbilities (TheEndOfAllThings x) =
    [ mkAbility x 1 $ ForcedAbility $ MovedBy
      Timing.After
      You
      EncounterCardSource
    , mkAbility x 2
    $ ForcedAbility
    $ EnemyDefeated Timing.When Anyone
    $ EnemyWithTitle "Yog-Sothoth"
    ]

instance (HasId (Maybe EnemyId) env EnemyMatcher, AgendaRunner env) => RunMessage env TheEndOfAllThings where
  runMessage msg a@(TheEndOfAllThings attrs@AgendaAttrs {..}) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (InvestigatorAssignDamage iid source DamageAny 0 1)
    UseCardAbility _ source _ 2 _ | isSource attrs source ->
      a <$ push (ScenarioResolution $ Resolution 3)
    AdvanceAgenda aid | aid == agendaId && onSide B attrs -> do
      investigatorIds <- getInvestigatorIds
      yogSothoth <- fromJustNote "defeated?"
        <$> getId (EnemyWithTitle "Yog-Sothoth")
      a <$ pushAll
        ([ EnemyAttack iid yogSothoth DamageAny | iid <- investigatorIds ]
        <> [RevertAgenda aid]
        )
    _ -> TheEndOfAllThings <$> runMessage msg attrs
