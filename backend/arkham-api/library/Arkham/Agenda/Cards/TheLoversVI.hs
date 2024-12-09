module Arkham.Agenda.Cards.TheLoversVI (
  TheLoversVI (..),
  theLoversVI,
  theLoversVIEffect,
) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.Classes
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (EnemyTokens))
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Projection
import Arkham.Token
import Arkham.Zone

newtype TheLoversVI = TheLoversVI AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theLoversVI :: AgendaCard TheLoversVI
theLoversVI = agenda (1, A) TheLoversVI Cards.theLoversVI (Static 8)

instance RunMessage TheLoversVI where
  runMessage msg a@(TheLoversVI attrs) =
    case msg of
      AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
        takenByTheWatcher <- length <$> getRecordSet WasTakenByTheWatcher
        if takenByTheWatcher > 0
          then do
            card <- genFlippedCard attrs
            theSpectralWatcher <-
              fromJustNote "missing the spectral watcher"
                <$> ( liftA2
                        (<|>)
                        (selectOne $ OutOfPlayEnemy SetAsideZone $ enemyIs Enemies.theSpectralWatcher)
                        (selectOne $ enemyIs Enemies.theSpectralWatcher)
                    )
            enabled <- createCardEffect Cards.theLoversVI Nothing attrs ScenarioTarget
            pushAll
              [ PlaceTokens (toSource attrs) (toTarget theSpectralWatcher) LostSoul takenByTheWatcher
              , PlaceNextTo AgendaDeckTarget [card]
              , enabled
              , advanceAgendaDeck attrs
              ]
          else pushAll [advanceAgendaDeck attrs]
        push ShuffleEncounterDiscardBackIn
        pure a
      _ -> TheLoversVI <$> runMessage msg attrs

newtype TheLoversVIEffect = TheLoversVIEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theLoversVIEffect :: EffectArgs -> TheLoversVIEffect
theLoversVIEffect = cardEffect TheLoversVIEffect Cards.theLoversVI

instance HasModifiersFor TheLoversVIEffect where
  getModifiersFor (TheLoversVIEffect attrs) = do
    modifySelectMaybe attrs (enemyIs Enemies.theSpectralWatcher) \eid -> do
      tokens <- lift $ fieldMap EnemyTokens (countTokens LostSoul) eid
      pure [EnemyFight tokens, HealthModifier tokens]

instance RunMessage TheLoversVIEffect where
  runMessage msg (TheLoversVIEffect attrs) = TheLoversVIEffect <$> runMessage msg attrs
