{- | The Soldier, unmasked. He retaliates even exhausted, and the only way to
blunt him is to bleed every sigil at once.

"{fast} Spend 1 offering from each copy of Antediluvian Sigils" is a cost no
'Cost' constructor can express: 'UseCost' takes one asset from a matcher,
however many copies match, and @getAbilities@ is pure so the copies cannot be
enumerated into a 'Costs' list. The ability keeps its {fast} type and its
printed criteria -- it is only offered while every copy has an offering to give
-- and the spend is pushed first in the handler. The visible difference is that
it resolves as an effect rather than a payment; an engine-level
@UseCostOfEach@ would close it.
-}
module Arkham.Homebrew.AndThenThereWereNone.Enemies.PhilipLombardSnakeInTheGrass (
  philipLombardSnakeInTheGrass,
) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.AndThenThereWereNone.Helpers
import Arkham.Keyword (Keyword (Retaliate))
import Arkham.Matcher

newtype PhilipLombardSnakeInTheGrass = PhilipLombardSnakeInTheGrass EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

philipLombardSnakeInTheGrass :: EnemyCard PhilipLombardSnakeInTheGrass
philipLombardSnakeInTheGrass = enemy PhilipLombardSnakeInTheGrass Cards.philipLombardSnakeInTheGrass

instance HasModifiersFor PhilipLombardSnakeInTheGrass where
  -- "Philip Lombard can retaliate while exhausted." (Retaliate itself is a
  -- printed keyword on the def.)
  getModifiersFor (PhilipLombardSnakeInTheGrass a) = modifySelf a [CanRetaliateWhileExhausted]

-- | Every copy in play still has an offering to spend.
everySigilHasAnOffering :: Criterion
everySigilHasAnOffering =
  exists antediluvianSigils
    <> not_ (exists $ antediluvianSigils <> AssetWithSpendableUses (lessThan 1) #offering)

instance HasAbilities PhilipLombardSnakeInTheGrass where
  getAbilities (PhilipLombardSnakeInTheGrass a) =
    extend1 a
      $ scenarioI18n
      $ withI18nTooltip "philipLombardSnakeInTheGrass.blunt"
      $ restricted a 1 everySigilHasAnOffering
      $ FastAbility Free

instance RunMessage PhilipLombardSnakeInTheGrass where
  runMessage msg e@(PhilipLombardSnakeInTheGrass attrs) = runQueueT $ case msg of
    -- "Philip Lombard gets -2 fight and loses retaliate until the end of the round."
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectEach antediluvianSigils \aid -> spendUses (attrs.ability 1) aid #offering 1
      roundModifiers (attrs.ability 1) attrs [EnemyFight (-2), RemoveKeyword Retaliate]
      pure e
    _ -> PhilipLombardSnakeInTheGrass <$> liftRunMessage msg attrs
