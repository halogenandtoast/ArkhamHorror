module Arkham.Enemy.Cards.Subject8L08EpicMultiplayer (subject8L08EpicMultiplayer) where

import Arkham.Ability
import Arkham.Card (toCardId)
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Epic.Types (SharedKey (SharedEnemyHealth), sharedKeyText)
import Arkham.Helpers.Log (scenarioCount)
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Modifier (UIModifier (Oversized))
import Arkham.ScenarioLogKey (ScenarioCountKey (EpicShared))
import Arkham.Token qualified as Token
import Arkham.Trait (toTraits)

-- Epic Multiplayer variant of Subject 8L-08 (card 85037). Its health is a single
-- GLOBAL pool shared across every group in the event. The pool lives on the
-- locked epic-event row; at the start of every action @updateGame@ mirrors the
-- current remaining value into this group's scenario state as the
-- @EpicShared "enemy-health:85037"@ count, which we read purely here. Damage in
-- any group drains the global pool (via a captured @SpendShared@ delta) rather
-- than accumulating locally; the blob is defeated everywhere when the pool hits
-- 0 (mirroring the Single Group Objective ability -> R2).
newtype Subject8L08EpicMultiplayer = Subject8L08EpicMultiplayer EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- The shared-health 'SharedKey' for this enemy, and the textual scenario-count
-- key it is mirrored under.
sharedHealthKey :: EnemyAttrs -> SharedKey
sharedHealthKey _ = SharedEnemyHealth Cards.subject8L08EpicMultiplayer.cardCode

instance HasModifiersFor Subject8L08EpicMultiplayer where
  getModifiersFor (Subject8L08EpicMultiplayer a) = do
    -- Surface the shared remaining pool as this copy's health number, so all
    -- groups see the same global value drop. The printed health is @*@ (base 0),
    -- so HealthModifier sets the absolute value.
    remaining <- scenarioCount (EpicShared (sharedKeyText (sharedHealthKey a)))
    modifySelf a [UIModifier Oversized, HealthModifier (max 0 remaining)]

instance HasAbilities Subject8L08EpicMultiplayer where
  getAbilities (Subject8L08EpicMultiplayer a) =
    [mkAbility a 1 $ Objective $ forced $ EnemyDefeated #when Anyone ByAny (be a)]

subject8L08EpicMultiplayer :: EnemyCard Subject8L08EpicMultiplayer
subject8L08EpicMultiplayer =
  enemyWith Subject8L08EpicMultiplayer Cards.subject8L08EpicMultiplayer
    $ \a -> a {enemyFight = Nothing, enemyEvade = Nothing}

instance RunMessage Subject8L08EpicMultiplayer where
  runMessage msg e@(Subject8L08EpicMultiplayer attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push R2
      pure e
    ScenarioCountSet (EpicShared key) v | key == sharedKeyText (sharedHealthKey attrs) -> do
      -- Start of action: the shared remaining health was just mirrored in. The
      -- per-copy damage we let accumulate is only a *this-action* tally (it drove
      -- the killing-blow check below), so reset it to 0 here; cross-action
      -- accounting lives entirely in the global pool. If the pool is already
      -- exhausted, this group lands no blow yet still loses its blob: defeat the
      -- local copy (pull), which fires the Objective -> R2.
      when (v <= 0 && not (enemyDefeated attrs))
        $ push
        $ Defeated (toTarget attrs) (toCardId attrs) GameSource (setToList $ toTraits attrs)
      pure $ Subject8L08EpicMultiplayer (attrs & tokensL %~ Token.removeAllTokens Token.Damage)
    CheckDefeated _ (isTarget attrs -> True) -> do
      -- Let the engine apply the damage and decide defeat against the
      -- HealthModifier-driven health (= shared remaining at action start). Then
      -- drain the global pool by exactly the amount applied. With the per-action
      -- reset above this means: this copy is defeated iff a single group deals at
      -- least the whole remaining pool in one action (the killing blow); a group
      -- can never kill it from its own accumulated local damage alone.
      attrs' <- liftRunMessage msg attrs
      let applied = attrs'.damage - attrs.damage
      when (applied > 0) $ push $ SpendShared (sharedHealthKey attrs) applied
      pure $ Subject8L08EpicMultiplayer attrs'
    _ -> Subject8L08EpicMultiplayer <$> liftRunMessage msg attrs
