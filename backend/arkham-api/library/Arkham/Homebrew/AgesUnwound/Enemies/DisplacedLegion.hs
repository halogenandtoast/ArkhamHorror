module Arkham.Homebrew.AgesUnwound.Enemies.DisplacedLegion (displacedLegion) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.AgesUnwound.Helpers (campaignI18n)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher

newtype DisplacedLegion = DisplacedLegion EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | /Displaced Legion/ (@:ages-unwound:124@). A Roman cohort stranded in 1924:
Massive, 8 health, and not meant to be fought. Talking it down with the Parley
makes it aloof for the round, and only then can the portal be opened -- which is
the only way to claim its victory point.
-}
displacedLegion :: EnemyCard DisplacedLegion
displacedLegion = enemy DisplacedLegion Cards.displacedLegion

{- | "[action]: __Parley.__ Test [intellect] (4) to communicate with the
soldiers. If you succeed, until the end of the round, Displaced Legion loses
massive and gains aloof. / [action] If Displaced Legion is aloof: Test
[willpower] (4) to open a portal through time. If you succeed, add Displaced
Legion to the victory display."

Ability 2's "is aloof" is read through 'EnemyWithKeyword', which folds keyword
modifiers over the printed keywords -- so the round-long grant from ability 1 is
what unlocks it, and nothing has to remember that the Parley happened.
-}
instance HasAbilities DisplacedLegion where
  getAbilities (DisplacedLegion a) =
    extend
      a
      [ skillTestAbility
          $ campaignI18n
          $ withI18nTooltip "displacedLegion.parley"
          $ restricted a 1 OnSameLocation
          $ ActionAbility #parley #intellect (ActionCost 1)
      , skillTestAbility
          $ campaignI18n
          $ withI18nTooltip "displacedLegion.portal"
          $ restricted a 2 (OnSameLocation <> thisExists a (EnemyWithKeyword Keyword.Aloof)) actionAbility
      ]

instance RunMessage DisplacedLegion where
  runMessage msg e@(DisplacedLegion attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #intellect (Fixed 4)
      pure e
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      roundModifiers (attrs.ability 1) attrs [RemoveKeyword Keyword.Massive, AddKeyword Keyword.Aloof]
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 2) attrs #willpower (Fixed 4)
      pure e
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      addToVictory iid attrs
      pure e
    _ -> DisplacedLegion <$> liftRunMessage msg attrs
