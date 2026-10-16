module Arkham.Homebrew.TheInvisibleMan.Enemies.MaddenedBeast (maddenedBeast) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Homebrew.TheInvisibleMan.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.TheInvisibleMan.Helpers
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher

newtype MaddenedBeast = MaddenedBeast EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

maddenedBeast :: EnemyCard MaddenedBeast
maddenedBeast = enemy MaddenedBeast Cards.maddenedBeast

{- | "...if Maddened Beast did not move". 'enemyMovedFromHunterKeyword' is the
engine's own record of the hunter move: it is set only once a move has actually
been queued and is cleared at the end of the phase, so the printed ability is
left out of the list exactly while it must not fire.
-}
instance HasAbilities MaddenedBeast where
  getAbilities (MaddenedBeast a) =
    extend
      a
      [ scenarioI18n
          $ withI18nTooltip "maddenedBeast.frenzy"
          $ mkAbility a 1
          $ forced
          $ Matcher.PhaseStep #after HuntersMoveStep
      | not (enemyMovedFromHunterKeyword a)
      ]

instance RunMessage MaddenedBeast where
  runMessage msg e@(MaddenedBeast attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      roundModifier (attrs.ability 1) attrs (DamageDealt 1)
      pure e
    _ -> MaddenedBeast <$> liftRunMessage msg attrs
