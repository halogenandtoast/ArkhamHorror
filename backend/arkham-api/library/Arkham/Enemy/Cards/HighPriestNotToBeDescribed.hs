module Arkham.Enemy.Cards.HighPriestNotToBeDescribed (
  highPriestNotToBeDescribed,
  HighPriestNotToBeDescribed (..),
)
where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher
import Arkham.ScenarioLogKey

newtype HighPriestNotToBeDescribed = HighPriestNotToBeDescribed EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

highPriestNotToBeDescribed :: EnemyCard HighPriestNotToBeDescribed
highPriestNotToBeDescribed = enemy HighPriestNotToBeDescribed Cards.highPriestNotToBeDescribed (5, PerPlayer 3, 3) (1, 1)

instance HasModifiersFor HighPriestNotToBeDescribed where
  getModifiersFor (HighPriestNotToBeDescribed a) = modifySelf a [CannotMakeAttacksOfOpportunity]

instance HasAbilities HighPriestNotToBeDescribed where
  getAbilities (HighPriestNotToBeDescribed attrs) =
    extend
      attrs
      [ restrictedAbility
          attrs
          1
          (OnSameLocation <> exists (be attrs <> ExhaustedEnemy) <> not_ (Remembered StunnedThePriest))
          $ FastAbility (ClueCost $ PerPlayer 1)
      ]

instance RunMessage HighPriestNotToBeDescribed where
  runMessage msg e@(HighPriestNotToBeDescribed attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      remember StunnedThePriest
      gameModifier (attrs.ability 1) attrs (EnemyFight (-3))
      pure e
    _ -> HighPriestNotToBeDescribed <$> liftRunMessage msg attrs
