module Arkham.Enemy.Cards.CthulhuFierceVisageEnraged (cthulhuFierceVisageEnraged) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyEvaded)
import Arkham.Helpers.Message.Discard.Lifted (randomDiscard)
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Scenarios.TheDoomOfArkhamPartII.Helpers (getCthulhuRage)

newtype CthulhuFierceVisageEnraged = CthulhuFierceVisageEnraged EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- Keywords Alert and Retaliate are on the card def. Prey - Highest [willpower].
cthulhuFierceVisageEnraged :: EnemyCard CthulhuFierceVisageEnraged
cthulhuFierceVisageEnraged =
  enemy CthulhuFierceVisageEnraged Cards.cthulhuFierceVisageEnraged
    & setPrey (InvestigatorWithHighestSkill #willpower UneliminatedInvestigator)

instance HasModifiersFor CthulhuFierceVisageEnraged where
  getModifiersFor (CthulhuFierceVisageEnraged a) = do
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

instance HasAbilities CthulhuFierceVisageEnraged where
  getAbilities (CthulhuFierceVisageEnraged a) =
    extend
      a
      [ {- "Forced - After you flip this enemy to this side: Discard 1 card at random
        from your hand." The non-Enraged side raises this window as it swaps itself
        out, so the effect fires from the side that prints it. -}
        mkAbility a 1 $ forced $ EnemyFlipped #after (be a)
      , -- "[reaction] After you evade this enemy: Deal 1 damage to it."
        mkAbility a 2 $ freeReaction $ EnemyEvaded #after You (be a)
      ]

instance RunMessage CthulhuFierceVisageEnraged where
  runMessage msg e@(CthulhuFierceVisageEnraged attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      randomDiscard iid (attrs.ability 1)
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      nonAttackEnemyDamage (Just iid) (attrs.ability 2) 1 attrs
      pure e
    _ -> CthulhuFierceVisageEnraged <$> liftRunMessage msg attrs
