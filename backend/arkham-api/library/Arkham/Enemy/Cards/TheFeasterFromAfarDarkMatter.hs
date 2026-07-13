module Arkham.Enemy.Cards.TheFeasterFromAfarDarkMatter (theFeasterFromAfarDarkMatter) where

import Arkham.Ability
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Scenario.Deck

newtype TheFeasterFromAfarDarkMatter = TheFeasterFromAfarDarkMatter EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theFeasterFromAfarDarkMatter :: EnemyCard TheFeasterFromAfarDarkMatter
theFeasterFromAfarDarkMatter = enemy TheFeasterFromAfarDarkMatter Cards.theFeasterFromAfarDarkMatter

instance HasModifiersFor TheFeasterFromAfarDarkMatter where
  getModifiersFor (TheFeasterFromAfarDarkMatter a) = modifySelf a [AddKeyword Keyword.Massive]

instance HasAbilities TheFeasterFromAfarDarkMatter where
  getAbilities (TheFeasterFromAfarDarkMatter a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyAttacks #after You AnyEnemyAttack (be a)

instance RunMessage TheFeasterFromAfarDarkMatter where
  runMessage msg e@(TheFeasterFromAfarDarkMatter attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ HealAllDamage (toTarget attrs) (attrs.ability 1)
      -- "place it at the bottom of the scanning deck": modeled as shuffling the
      -- card back into the scanning deck (which is reshuffled during scans), so
      -- the enemy is removed from play and returns to the scanning pool.
      shuffleIntoDeck (Deck.ScenarioDeckByKey ScanningDeck) attrs
      pure e
    _ -> TheFeasterFromAfarDarkMatter <$> liftRunMessage msg attrs
