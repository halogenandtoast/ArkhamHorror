module Arkham.Enemy.Cards.BrotherhoodAcolyte (brotherhoodAcolyte) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Trait (Trait (Cultist))

newtype BrotherhoodAcolyte = BrotherhoodAcolyte EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

brotherhoodAcolyte :: EnemyCard BrotherhoodAcolyte
brotherhoodAcolyte =
  enemy BrotherhoodAcolyte Cards.brotherhoodAcolyte (3, Static 1, 1) (0, 1)
    & setSpawnAt (LocationWithMostEnemies Anywhere (EnemyWithTrait Cultist))

instance HasAbilities BrotherhoodAcolyte where
  getAbilities (BrotherhoodAcolyte a) =
    extend1 a
      $ restricted
        a
        1
        ( exists
            $ EnemyAt (locationWithEnemy a)
            <> not_ (be a)
            <> EnemyWithTrait Cultist
            <> not_ (EnemyWithModifier CannotPlaceDoomOnThis)
        )
      $ forced
      $ EnemySpawns #after Anywhere (be a)

instance RunMessage BrotherhoodAcolyte where
  runMessage msg e@(BrotherhoodAcolyte attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      enemies <-
        select
          $ EnemyAt (locationWithEnemy attrs)
          <> not_ (be attrs)
          <> EnemyWithTrait Cultist
          <> not_ (EnemyWithModifier CannotPlaceDoomOnThis)
      for_ enemies (placeDoomOn (attrs.ability 1) 1)
      pure e
    _ -> BrotherhoodAcolyte <$> liftRunMessage msg attrs
