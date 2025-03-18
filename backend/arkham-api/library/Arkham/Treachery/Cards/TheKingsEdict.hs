module Arkham.Treachery.Cards.TheKingsEdict (theKingsEdict, theKingsEdictEffect) where

import Arkham.Effect.Import
import Arkham.Enemy.Types (Field (EnemyClues, EnemyDoom))
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Projection
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype TheKingsEdict = TheKingsEdict TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theKingsEdict :: TreacheryCard TheKingsEdict
theKingsEdict = treachery TheKingsEdict Cards.theKingsEdict

instance RunMessage TheKingsEdict where
  runMessage msg t@(TheKingsEdict attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      select (EnemyWithTrait Cultist <> at_ LocationWithAnyClues) >>= \case
        [] -> gainSurge attrs
        xs -> for_ xs \cultist ->
          selectForMaybeM (locationWithEnemy cultist) \lid -> moveTokens attrs lid cultist #clue 1
      selectEach (InPlayEnemy $ withTrait Cultist)
        $ createCardEffect Cards.theKingsEdict Nothing attrs
      pure t
    _ -> TheKingsEdict <$> liftRunMessage msg attrs

newtype TheKingsEdictEffect = TheKingsEdictEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theKingsEdictEffect :: EffectArgs -> TheKingsEdictEffect
theKingsEdictEffect = cardEffect TheKingsEdictEffect Cards.theKingsEdict

instance HasModifiersFor TheKingsEdictEffect where
  getModifiersFor (TheKingsEdictEffect a) = for_ a.target.enemy \eid ->
    maybeModified_ a eid do
      clueCount <- MaybeT $ fieldMay EnemyClues eid
      doomCount <- MaybeT $ fieldMay EnemyDoom eid
      guard $ clueCount + doomCount > 0
      pure [EnemyFight (clueCount + doomCount)]

instance RunMessage TheKingsEdictEffect where
  runMessage msg e@(TheKingsEdictEffect attrs) = runQueueT $ case msg of
    RemovedFromPlay (EnemySource eid) -> case attrs.target of
      EnemyTarget eid' | eid == eid' -> disableReturn e
      _ -> pure e
    EndRound -> disableReturn e
    _ -> TheKingsEdictEffect <$> liftRunMessage msg attrs
