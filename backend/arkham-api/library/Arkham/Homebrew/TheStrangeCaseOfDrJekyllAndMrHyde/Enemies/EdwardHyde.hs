{- | Edward Hyde is the far side of the Henry Jekyll asset, so he arrives and
leaves only by the flip; see the scenario's Helpers for the card the two faces
share and for the horror that persists across it.

"__Forced__ - When Edward Hyde is defeated: Instead of discarding him, heal all
damage from him, disengage him from all investigators, and exhaust him." This
/replaces/ the defeat's disposal rather than adding to it, so it hangs off the
@#when@ window with 'insteadOfDiscarding': the defeat still happened and its
windows still fire, but the card never reaches the discard. The condition rides
the window ('be a') rather than an ability criterion, which at @#after@ would
silently never match. Only damage is healed -- the horror is the scenario's
clock and stays on the card.

"{fast} If Edward Hyde is exhausted: /Parley./ Test {willpower} or {intellect}
(2)." The card prints no location clause, but every official Parley printed on
an enemy card is restricted to investigators at that enemy's location (Sgt. Earl
Monroe, Abigail Foreman, Priscilla Thomas, Memory of an Unspeakable Evil), so
'OnSameLocation' is the reading taken here.
-}
module Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Enemies.EdwardHyde (edwardHyde) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Enemy (disengageEnemyFromAll, insteadOfDiscarding)
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Helpers hiding (edwardHyde)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype EdwardHyde = EdwardHyde EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | "__Patrol__ - Location without investigators." is a printed keyword on the def.
edwardHyde :: EnemyCard EdwardHyde
edwardHyde = enemy EdwardHyde Cards.edwardHyde

instance HasAbilities EdwardHyde where
  getAbilities (EdwardHyde a) =
    extend
      a
      [ scenarioI18n
          $ withI18nTooltip "edwardHyde.endure"
          $ mkAbility a 1
          $ forced
          $ EnemyDefeated #when Anyone ByAny (be a)
      , scenarioI18n
          $ withI18nTooltip "edwardHyde.parley"
          $ skillTestAbility
          $ restricted a 2 (OnSameLocation <> thisExists a ExhaustedEnemy)
          $ FastAbility' Free #parley
      ]

instance RunMessage EdwardHyde where
  runMessage msg e@(EdwardHyde attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      insteadOfDiscarding attrs do
        push $ HealAllDamage (toTarget attrs) (attrs.ability 1)
        disengageEnemyFromAll attrs
        exhaustThis attrs
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      let source = attrs.ability 2
      sid <- getRandom
      -- "This test gets +1 difficulty for each horror on Edward Hyde." A
      -- modifier rather than a bigger printed difficulty, so it stays visible
      -- and cancellable.
      horror <- hydeHorror
      when (horror > 0) $ skillTestModifier sid source sid (Difficulty horror)
      chooseSkillM iid [#willpower, #intellect] \sType ->
        parley sid iid source attrs sType (Fixed 2)
      pure e
    PassedThisSkillTest _ (isAbilitySource attrs 2 -> True) -> do
      flipToJekyll (attrs.ability 2)
      pure e
    FailedThisSkillTest _ (isAbilitySource attrs 2 -> True) -> do
      readyThis attrs
      pure e
    _ -> EdwardHyde <$> liftRunMessage msg attrs
