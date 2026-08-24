module Arkham.Homebrew.DarkMatter.Enemies.HoundOfTindalos (houndOfTindalos) where

import Arkham.Ability
import Arkham.Constants (pattern AbilityAttack, pattern AbilityEvade)
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modified_, modifySelect, modifySelf)
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Cards
import Arkham.Investigator.Types (Field (InvestigatorHand))
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Placement
import Arkham.Projection

newtype HoundOfTindalos = HoundOfTindalos EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | "Spawn - Put Hound of Tindalos into play next to the act deck, at no specific
location."

The placement is given directly rather than as a 'SpawnAt' location matcher: a
matcher that finds no location discards the spawning enemy, so "at no specific
location" would send the Hound straight to the discard pile.
-}
houndOfTindalos :: EnemyCard HoundOfTindalos
houndOfTindalos =
  enemyWith HoundOfTindalos Cards.houndOfTindalos (spawnAtL ?~ SpawnPlaced NextToAct)

{- | "Massive. Alert. Retaliate. / While you have an odd number of cards in your
hand, Hound of Tindalos is considered to be engaged with you."

The Hound is at no location, so the 'OnSameLocation' check that the basic fight
and evade abilities share would stop even an engaged investigator from acting
against it. Both are relaxed per investigator through 'SetAbilityCriteria' — the
one hook 'getCanPerformAbility' consults, and the only one that works when the
ability's source /is/ the enemy. Scoping falls out of who gets the modifier at
all, so the replacements need not re-check engagement.
-}
instance HasModifiersFor HoundOfTindalos where
  getModifiersFor (HoundOfTindalos a) = do
    modifySelf a [AddKeyword Keyword.Massive, AddKeyword Keyword.Alert, AddKeyword Keyword.Retaliate]
    -- Lost Quantum can leave a copy sitting face down in a threat area, where it
    -- is not in play and engages nobody.
    when (isInPlayPlacement a.placement) do
      investigators <- select UneliminatedInvestigator
      odds <- filterM (fieldMap InvestigatorHand (odd . length)) investigators
      for_ odds \iid -> do
        modifySelect a (InvestigatorWithId iid) [AsIfEngagedWith a.id]
        modified_ a (abilityTarget iid AbilityAttack) [SetAbilityCriteria $ CriteriaOverride fightCriteria]
        modified_ a (abilityTarget iid AbilityEvade) [SetAbilityCriteria $ CriteriaOverride evadeCriteria]
   where
    abilityTarget iid = AbilityTarget iid . AbilityRef (toSource a)
    -- 'canFightCriteria' and 'EvadeCriteria' minus their OnSameLocation check.
    fightCriteria = EnemyCriteria (ThisEnemy $ CanBeAttackedBy You) <> CanAttack
    evadeCriteria =
      EnemyCriteria
        (ThisEnemy $ EnemyMatchAll [EnemyWithEvade, EnemyWithoutModifier CannotBeEvaded])

instance RunMessage HoundOfTindalos where
  runMessage msg (HoundOfTindalos attrs) = HoundOfTindalos <$> runMessage msg attrs
