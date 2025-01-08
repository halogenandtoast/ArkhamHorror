module Arkham.Agenda.Cards.TheLoversVI (theLoversVI, theLoversVIEffect) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.TheCircleUndone.Key
import Arkham.Card
import Arkham.Effect.Import
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (EnemyTokens))
import Arkham.Matcher
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectMaybe)
import Arkham.Helpers.Log (getRecordSet)
import Arkham.Projection
import Arkham.Token
import Arkham.Zone

newtype TheLoversVI = TheLoversVI AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theLoversVI :: AgendaCard TheLoversVI
theLoversVI = agenda (1, A) TheLoversVI Cards.theLoversVI (Static 8)

instance RunMessage TheLoversVI where
  runMessage msg a@(TheLoversVI attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      shuffleEncounterDiscardBackIn
      takenByTheWatcher <- length <$> getRecordSet WasTakenByTheWatcher
      when (takenByTheWatcher > 0) do
        card <- genFlippedCard attrs
        theSpectralWatcher <-
          fromJustNote "missing the spectral watcher"
            <$> liftA2
              (<|>)
              (selectOne $ OutOfPlayEnemy SetAsideZone $ enemyIs Enemies.theSpectralWatcher)
              (selectOne $ enemyIs Enemies.theSpectralWatcher)
        placeTokens attrs theSpectralWatcher LostSoul takenByTheWatcher
        push $ PlaceNextTo AgendaDeckTarget [card]
        createCardEffect Cards.theLoversVI Nothing attrs ScenarioTarget
      advanceAgendaDeck attrs
      pure a
    _ -> TheLoversVI <$> liftRunMessage msg attrs

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
