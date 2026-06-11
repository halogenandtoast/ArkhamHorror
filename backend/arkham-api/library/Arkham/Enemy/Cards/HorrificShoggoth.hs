module Arkham.Enemy.Cards.HorrificShoggoth (horrificShoggoth) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher
import Arkham.Scenarios.WarOfTheOuterGods.Helpers

newtype HorrificShoggoth = HorrificShoggoth EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

horrificShoggoth :: EnemyCard HorrificShoggoth
horrificShoggoth = enemy HorrificShoggoth Cards.horrificShoggoth (4, Static 4, 2) (0, 1)

instance HasModifiersFor HorrificShoggoth where
  getModifiersFor (HorrificShoggoth a) = do
    mutations <- getMutations a.id
    modifySelf a [DamageDealt mutations | mutations > 0]

instance HasAbilities HorrificShoggoth where
  getAbilities (HorrificShoggoth a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyAttacks #when Anyone AnyEnemyAttack (be a)

instance RunMessage HorrificShoggoth where
  runMessage msg e@(HorrificShoggoth attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeMutations (attrs.ability 1) attrs.id 1
      pure e
    -- When Horrific Shoggoth attacks during the warring step: place 1
    -- resource on it, as a mutation.
    ScenarioSpecific "warringAttackWindow" v | toResult v == attrs.id -> do
      placeMutations attrs attrs.id 1
      HorrificShoggoth <$> liftRunMessage msg attrs
    _ -> HorrificShoggoth <$> liftRunMessage msg attrs
