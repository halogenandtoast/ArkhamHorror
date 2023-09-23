module Arkham.Agenda.Cards.InEveryShadow (
  InEveryShadow (..),
  inEveryShadow,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.GameValue
import Arkham.Matcher hiding (InvestigatorDefeated)
import Arkham.Message
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype InEveryShadow = InEveryShadow AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

inEveryShadow :: AgendaCard InEveryShadow
inEveryShadow = agenda (3, A) InEveryShadow Cards.inEveryShadow (Static 7)

instance HasAbilities InEveryShadow where
  getAbilities (InEveryShadow x) =
    [ mkAbility x 1
        $ ForcedAbility
        $ EnemySpawns Timing.When Anywhere
        $ enemyIs
          Cards.huntingHorror
    ]

instance RunMessage InEveryShadow where
  runMessage msg a@(InEveryShadow attrs) = case msg of
    UseCardAbility _ source 1 [(windowType -> Window.EnemySpawns eid _)] _ | isSource attrs source -> do
      mShadowSpawnedId <- selectOne $ treacheryIs Treacheries.shadowSpawned
      shadowSpawned <- genCard Treacheries.shadowSpawned
      case mShadowSpawnedId of
        Just tid -> push $ PlaceResources (toAbilitySource attrs 1) (TreacheryTarget tid) 1
        Nothing -> push $ AttachStoryTreacheryTo shadowSpawned (EnemyTarget eid)
      pure a
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      iids <- selectList UneliminatedInvestigator
      pushAll
        $ concatMap
          ( \iid ->
              [SufferTrauma iid 1 0, InvestigatorDefeated (toSource attrs) iid]
          )
          iids
      pure a
    _ -> InEveryShadow <$> runMessage msg attrs
