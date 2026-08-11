module Arkham.Homebrew.DarkMatter.Enemies.CaveDweller (caveDweller) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Cards
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher

newtype CaveDweller = CaveDweller EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

caveDweller :: EnemyCard CaveDweller
caveDweller = enemy CaveDweller Cards.caveDweller

-- | "Massive. Retaliate. Hunter."
instance HasModifiersFor CaveDweller where
  getModifiersFor (CaveDweller a) =
    modifySelf a [AddKeyword Keyword.Massive, AddKeyword Keyword.Retaliate, AddKeyword Keyword.Hunter]

-- | "Forced - When Cave Dweller's location is flipped: Exhaust Cave Dweller."
instance HasAbilities CaveDweller where
  getAbilities (CaveDweller a) =
    extend1 a $ mkAbility a 1 $ forced $ FlipLocation #when Anyone (locationWithEnemy a.id)

instance RunMessage CaveDweller where
  runMessage msg e@(CaveDweller attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      exhaustThis attrs
      pure e
    _ -> CaveDweller <$> liftRunMessage msg attrs
