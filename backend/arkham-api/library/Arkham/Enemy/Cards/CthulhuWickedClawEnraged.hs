module Arkham.Enemy.Cards.CthulhuWickedClawEnraged (cthulhuWickedClawEnraged) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyEvaded)
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Scenarios.TheDoomOfArkhamPartII.Helpers (getCthulhuRage)

newtype CthulhuWickedClawEnraged = CthulhuWickedClawEnraged EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- Keyword Retaliate is on the card def. Prey - Highest [combat].
cthulhuWickedClawEnraged :: EnemyCard CthulhuWickedClawEnraged
cthulhuWickedClawEnraged =
  enemy CthulhuWickedClawEnraged Cards.cthulhuWickedClawEnraged
    & setPrey (InvestigatorWithHighestSkill #combat UneliminatedInvestigator)

instance HasModifiersFor CthulhuWickedClawEnraged where
  getModifiersFor (CthulhuWickedClawEnraged a) = do
    {- "X is Cthulhu's Rage." On this side that covers health as well as fight and
    evade — all three are printed X — which is what makes an Enraged facet killable
    at all. Each printed X calculates to 0, so the Rage is added to zero. -}
    rage <- getCthulhuRage
    modifySelf
      a
      [ EnemyFight rage
      , EnemyEvade rage
      , HealthModifier rage
      , {- "Cannot be exhausted." Note that, unlike the non-Enraged side, this one
        omits "cannot be damaged, defeated" — being damageable is how a facet
        reaches the victory display. DoNotExhaustEvaded covers the exhaust a
        successful evade would otherwise apply. -}
        DoNotExhaust
      , DoNotExhaustEvaded
      ]

instance HasAbilities CthulhuWickedClawEnraged where
  getAbilities (CthulhuWickedClawEnraged a) =
    extend
      a
      [ {- "Forced - After you flip this enemy to this side: Place 1 doom on it." The
        non-Enraged side raises this window as it swaps itself out, so the effect
        fires from the side that prints it. -}
        mkAbility a 1 $ forced $ EnemyFlipped #after (be a)
      , -- "[reaction] After you evade this enemy: Deal 1 damage to it."
        mkAbility a 2 $ freeReaction $ EnemyEvaded #after You (be a)
      ]

instance RunMessage CthulhuWickedClawEnraged where
  runMessage msg e@(CthulhuWickedClawEnraged attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeDoom (attrs.ability 1) attrs 1
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      nonAttackEnemyDamage (Just iid) (attrs.ability 2) 1 attrs
      pure e
    _ -> CthulhuWickedClawEnraged <$> liftRunMessage msg attrs
