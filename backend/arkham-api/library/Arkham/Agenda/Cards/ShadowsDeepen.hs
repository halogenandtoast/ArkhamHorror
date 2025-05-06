module Arkham.Agenda.Cards.ShadowsDeepen (shadowsDeepen) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query
import Arkham.Matcher
import Arkham.Scenarios.TheMiskatonicMuseum.Helpers
import Arkham.Treachery.Cards qualified as Treacheries

newtype ShadowsDeepen = ShadowsDeepen AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shadowsDeepen :: AgendaCard ShadowsDeepen
shadowsDeepen = agenda (2, A) ShadowsDeepen Cards.shadowsDeepen (Static 7)

instance HasAbilities ShadowsDeepen where
  getAbilities (ShadowsDeepen a) =
    [ groupLimit PerTestOrAbility
        $ restricted a 1 (oneOf [thisExists a (AgendaWithSide A), IsReturnTo])
        $ forced
        $ EnemySpawns #when Anywhere
        $ enemyIs Enemies.huntingHorror
    ]

instance RunMessage ShadowsDeepen where
  runMessage msg a@(ShadowsDeepen attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectOne (treacheryIs Treacheries.shadowSpawned) >>= \case
        Just tid -> placeTokens (attrs.ability 1) tid #resource 1
        Nothing -> do
          huntingHorror <- selectJust $ enemyIs Enemies.huntingHorror
          shadowSpawned <- fetchCard Treacheries.shadowSpawned
          tid <- getRandom
          push $ AttachStoryTreacheryTo tid shadowSpawned (toTarget huntingHorror)
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      getInPlayHuntingHorror >>= \case
        Just eid -> placeDoom attrs eid 1
        Nothing -> do
          lead <- getLead
          findEncounterCardIn lead attrs (cardIs Enemies.huntingHorror) [#deck, #discard, #void]
      advanceAgendaDeck attrs
      pure a
    FoundEnemyInOutOfPlay VoidZone _ (isTarget attrs -> True) eid -> do
      lid <- selectJust $ LocationWithTitle "Museum Halls"
      push $ EnemySpawnFromOutOfPlay VoidZone Nothing lid eid
      pure a
    FoundEncounterCard _ (isTarget attrs -> True) ec -> do
      lid <- selectJust $ LocationWithTitle "Museum Halls"
      push $ SpawnEnemyAt (EncounterCard ec) lid
      pure a
    _ -> ShadowsDeepen <$> liftRunMessage msg attrs
