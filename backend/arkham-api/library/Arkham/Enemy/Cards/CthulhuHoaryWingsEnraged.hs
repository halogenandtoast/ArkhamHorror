module Arkham.Enemy.Cards.CthulhuHoaryWingsEnraged (cthulhuHoaryWingsEnraged) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyEvaded)
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Scenarios.TheDoomOfArkhamPartII.Helpers (drawCthulhuDeckCard, getCthulhuRage)

newtype CthulhuHoaryWingsEnraged = CthulhuHoaryWingsEnraged EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- Keyword Alert is on the card def. Prey - Highest [agility].
cthulhuHoaryWingsEnraged :: EnemyCard CthulhuHoaryWingsEnraged
cthulhuHoaryWingsEnraged =
  enemy CthulhuHoaryWingsEnraged Cards.cthulhuHoaryWingsEnraged
    & setPrey (InvestigatorWithHighestSkill #agility UneliminatedInvestigator)

instance HasModifiersFor CthulhuHoaryWingsEnraged where
  getModifiersFor (CthulhuHoaryWingsEnraged a) = do
    rage <- getCthulhuRage
    modifySelf
      a
      [ EnemyFight rage
      , EnemyEvade rage
      , HealthModifier rage
      , DoNotExhaust
      , DoNotExhaustEvaded
      ]

instance HasAbilities CthulhuHoaryWingsEnraged where
  getAbilities (CthulhuHoaryWingsEnraged a) =
    extend
      a
      [ {- "Forced - After you flip this enemy to this side: Draw the top card of the
        Cthulhu deck." The non-Enraged side raises this window as it swaps itself
        out, so the effect fires from the side that prints it. -}
        mkAbility a 1 $ forced $ EnemyFlipped #after (be a)
      , -- "[reaction] After you evade this enemy: Deal 1 damage to it."
        mkAbility a 2 $ freeReaction $ EnemyEvaded #after You (be a)
      ]

instance RunMessage CthulhuHoaryWingsEnraged where
  runMessage msg e@(CthulhuHoaryWingsEnraged attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawCthulhuDeckCard iid (attrs.ability 1)
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      nonAttackEnemyDamage (Just iid) (attrs.ability 2) 1 attrs
      pure e
    _ -> CthulhuHoaryWingsEnraged <$> liftRunMessage msg attrs
