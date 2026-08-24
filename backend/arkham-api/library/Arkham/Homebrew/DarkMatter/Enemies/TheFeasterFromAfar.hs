module Arkham.Homebrew.DarkMatter.Enemies.TheFeasterFromAfar (theFeasterFromAfar) where

import Arkham.Ability
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.DarkMatter.ScenarioDeckKeys (pattern ScanningDeck)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher

newtype TheFeasterFromAfar = TheFeasterFromAfar EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theFeasterFromAfar :: EnemyCard TheFeasterFromAfar
theFeasterFromAfar = enemy TheFeasterFromAfar Cards.theFeasterFromAfar

instance HasModifiersFor TheFeasterFromAfar where
  getModifiersFor (TheFeasterFromAfar a) = modifySelf a [AddKeyword Keyword.Massive]

instance HasAbilities TheFeasterFromAfar where
  getAbilities (TheFeasterFromAfar a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyAttacks #after You AnyEnemyAttack (be a)

instance RunMessage TheFeasterFromAfar where
  runMessage msg e@(TheFeasterFromAfar attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ HealAllDamage (toTarget attrs) (attrs.ability 1)
      -- Not shuffled in: the position matters, since Scream of the Dead reads
      -- the top card of the scanning deck.
      putOnBottomOfDeck iid (Deck.ScenarioDeckByKey ScanningDeck) attrs
      pure e
    _ -> TheFeasterFromAfar <$> liftRunMessage msg attrs
