module Arkham.Agenda.Cards.TheEndOfAllThings (
  TheEndOfAllThings (..),
  theEndOfAllThings,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Attack
import Arkham.Classes
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Message hiding (EnemyDefeated)
import Arkham.Timing qualified as Timing

newtype TheEndOfAllThings = TheEndOfAllThings AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theEndOfAllThings :: AgendaCard TheEndOfAllThings
theEndOfAllThings =
  agenda (4, A) TheEndOfAllThings Cards.theEndOfAllThings (Static 2)

instance HasAbilities TheEndOfAllThings where
  getAbilities (TheEndOfAllThings x) =
    [ mkAbility x 1 $
        ForcedAbility $
          MovedBy
            Timing.After
            You
            Matcher.EncounterCardSource
    , mkAbility x 2 $
        ForcedAbility $
          EnemyDefeated Timing.When Anyone ByAny $
            EnemyWithTitle "Yog-Sothoth"
    ]

instance RunMessage TheEndOfAllThings where
  runMessage msg a@(TheEndOfAllThings attrs@AgendaAttrs {..}) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      a <$ push (InvestigatorAssignDamage iid source DamageAny 0 1)
    UseCardAbility _ source 2 _ _ | isSource attrs source -> do
      a <$ push (scenarioResolution 3)
    AdvanceAgenda aid | aid == agendaId && onSide B attrs -> do
      investigatorIds <- getInvestigatorIds
      yogSothoth <- selectJust (EnemyWithTitle "Yog-Sothoth")
      pushAll $
        map (EnemyAttack . enemyAttack yogSothoth attrs) investigatorIds
          <> [RevertAgenda aid]
      pure a
    _ -> TheEndOfAllThings <$> runMessage msg attrs
