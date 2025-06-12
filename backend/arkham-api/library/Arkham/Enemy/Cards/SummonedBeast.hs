module Arkham.Enemy.Cards.SummonedBeast (summonedBeast) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyDefeated)
import Arkham.Enemy.Types qualified as Field
import Arkham.Helpers.Doom
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query
import Arkham.Matcher
import Arkham.Trait (Trait (Humanoid))

newtype SummonedBeast = SummonedBeast EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

summonedBeast :: EnemyCard SummonedBeast
summonedBeast = enemy SummonedBeast Cards.summonedBeast (5, PerPlayer 6, 2) (2, 2)

instance HasModifiersFor SummonedBeast where
  getModifiersFor (SummonedBeast attrs) = do
    n <- (`div` 2) <$> getDoomCount
    modifySelf attrs [EnemyFight n, EnemyEvade n, DamageDealt n, HorrorDealt n]

instance HasAbilities SummonedBeast where
  getAbilities (SummonedBeast a) =
    extend
      a
      [ restricted a 1 (exists $ at_ (locationWithEnemy a) <> EnemyWithTrait Humanoid)
          $ forced
          $ PhaseBegins #when #enemy
      , mkAbility a 2 $ Objective $ forced $ EnemyDefeated #when Anyone ByAny (be a)
      ]

instance RunMessage SummonedBeast where
  runMessage msg e@(SummonedBeast attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      lead <- getLead
      humanoids <-
        selectWithField Field.EnemyDoom $ at_ (locationWithEnemy attrs) <> EnemyWithTrait Humanoid
      for_ humanoids \(humanoid, _) -> defeatEnemy humanoid lead (attrs.ability 1)
      placeDoom (attrs.ability 1) attrs (sum $ map snd humanoids)
      pure e
    UseCardAbility _ (isSource attrs -> True) 2 _ _ -> do
      addToVictory attrs
      push R3
      pure e
    _ -> SummonedBeast <$> liftRunMessage msg attrs
