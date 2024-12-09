module Arkham.Enemy.Cards.TheOrganistHopelessIDefiedHim (
  theOrganistHopelessIDefiedHim,
  TheOrganistHopelessIDefiedHim (..),
) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher

newtype TheOrganistHopelessIDefiedHim = TheOrganistHopelessIDefiedHim EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor TheOrganistHopelessIDefiedHim where
  getModifiersFor (TheOrganistHopelessIDefiedHim attrs) = modifySelf attrs [CannotBeDamaged]

instance HasAbilities TheOrganistHopelessIDefiedHim where
  getAbilities (TheOrganistHopelessIDefiedHim attrs) =
    extend1 attrs
      $ groupLimit PerRound
      $ mkAbility attrs 1
      $ forced
      $ MovedFromHunter #after (be attrs <> UnengagedEnemy)

theOrganistHopelessIDefiedHim :: EnemyCard TheOrganistHopelessIDefiedHim
theOrganistHopelessIDefiedHim =
  enemyWith TheOrganistHopelessIDefiedHim Cards.theOrganistHopelessIDefiedHim (5, Static 1, 3) (0, 3)
    $ healthL
    .~ Nothing

instance RunMessage TheOrganistHopelessIDefiedHim where
  runMessage msg e@(TheOrganistHopelessIDefiedHim attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      isEngaged <- selectAny $ investigatorEngagedWith attrs
      unless isEngaged do
        roundModifier (attrs.ability 1) attrs CannotAttack
        push $ HunterMove attrs.id
      pure e
    _ -> TheOrganistHopelessIDefiedHim <$> liftRunMessage msg attrs
