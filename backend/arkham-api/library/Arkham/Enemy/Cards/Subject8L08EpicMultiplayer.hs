module Arkham.Enemy.Cards.Subject8L08EpicMultiplayer (subject8L08EpicMultiplayer) where

import Arkham.Ability
import Arkham.Card (toCardId)
import Arkham.DamageEffect (damageAssignmentAmount)
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Epic.Types (SharedKey (SharedEnemyHealth), sharedKeyText, totalInvestigatorsKey)
import Arkham.Helpers.Log (scenarioCount)
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Modifier (UIModifier (Oversized))
import Arkham.ScenarioLogKey (ScenarioCountKey (EpicShared))
import Arkham.Token qualified as Token
import Arkham.Trait (toTraits)

-- Epic Multiplayer variant of Subject 8L-08 (card 85037). Its health is a single
-- GLOBAL pool shared across every group in the event, living on the locked
-- epic-event row. Health is presented as a FIXED maximum (15 per investigator,
-- across all groups) with the shared REMAINING pool surfaced as DAMAGE TOKENS
-- (max - remaining): at the start of every action @updateGame@ mirrors the
-- remaining value and the frozen total into this group's scenario state (as
-- @EpicShared@ counts), which we read purely here. Damage in any group drains the
-- global pool via a captured @SpendShared@ delta, so every group's board shows
-- the same climbing damage; the blob is defeated everywhere when the pool hits 0
-- (mirroring the Single Group Objective ability -> R2).
newtype Subject8L08EpicMultiplayer = Subject8L08EpicMultiplayer EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- Subject 8L-08's total health across the whole event scales by this per the
-- total participating investigator count.
healthMultiplierPerInvestigator :: Int
healthMultiplierPerInvestigator = 15

-- The shared-health 'SharedKey' for this enemy, and the textual scenario-count
-- key it is mirrored under.
sharedHealthKey :: EnemyAttrs -> SharedKey
sharedHealthKey _ = SharedEnemyHealth Cards.subject8L08EpicMultiplayer.cardCode

instance HasModifiersFor Subject8L08EpicMultiplayer where
  getModifiersFor (Subject8L08EpicMultiplayer a) = do
    -- Health is the FIXED maximum (15 per investigator across the whole event).
    -- The shared remaining pool is surfaced via DAMAGE TOKENS instead (see the
    -- ScenarioCountSet handler), so every group's board shows the same climbing
    -- accumulated-damage total against an identical max. The printed health is
    -- @*@ (base 0), so HealthModifier sets the absolute value.
    total <- scenarioCount (EpicShared totalInvestigatorsKey)
    modifySelf a [UIModifier Oversized, HealthModifier (healthMultiplierPerInvestigator * total)]

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
      -- Start of action: the shared REMAINING health (v) was just mirrored in; the
      -- frozen total was mirrored immediately before, so maxHealth is available.
      -- Surface the pool as DAMAGE TOKENS = accumulated damage = max - remaining,
      -- so every group's board shows the same climbing damage against the fixed
      -- max. If the pool is already exhausted, this group lands no blow yet still
      -- loses its blob: pull-defeat the local copy, which fires the Objective -> R2.
      maxHealth <- (healthMultiplierPerInvestigator *) <$> scenarioCount (EpicShared totalInvestigatorsKey)
      when (v <= 0 && not (enemyDefeated attrs))
        $ push
        $ Defeated (toTarget attrs) (toCardId attrs) GameSource (setToList $ toTraits attrs)
      pure $ Subject8L08EpicMultiplayer (attrs & tokensL %~ Token.setTokens Token.Damage (max 0 (maxHealth - v)))
    Damaged (isTarget attrs -> True) assignment -> do
      -- Damage to this copy drains the GLOBAL pool by the amount being dealt. We
      -- read it straight off the DamageAssignment: the engine's Damaged handler
      -- does NOT add the damage tokens synchronously (it queues a separate
      -- AssignedDamage), so diffing attrs.damage across liftRunMessage was always
      -- 0 and never drained the pool. The local tokens are then reconciled to the
      -- new global accumulated total (max - remaining) on the next ScenarioCountSet
      -- sync, and the killing blow lands when remaining hits 0.
      attrs' <- liftRunMessage msg attrs
      let amount = damageAssignmentAmount assignment
      when (amount > 0) $ push $ SpendShared (sharedHealthKey attrs) amount
      pure $ Subject8L08EpicMultiplayer attrs'
    _ -> Subject8L08EpicMultiplayer <$> liftRunMessage msg attrs
