module Arkham.Enemy.Cards.FrenziedMiner (frenziedMiner) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Trait (Trait (Resident))

newtype FrenziedMiner = FrenziedMiner EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

frenziedMiner :: EnemyCard FrenziedMiner
frenziedMiner = enemy FrenziedMiner Cards.frenziedMiner (0, Static 4, 0) (1, 0)

instance HasModifiersFor FrenziedMiner where
  getModifiersFor (FrenziedMiner a) = do
    withLocationOf a \lid -> do
      pos <- fieldJust LocationPosition lid
      modifySelf a [EnemyFight pos.column, EnemyEvade pos.column]

instance HasAbilities FrenziedMiner where
  getAbilities (FrenziedMiner a) =
    extend1 a
      $ mkAbility a 1 (forced $ EnemyAttacks #after You AnyEnemyAttack (be a))
      & restrict
        ( oneOf
            [ exists $ not_ You <> InvestigatorAt (locationWithEnemy a)
            , exists $ AssetAt (locationWithEnemy a) <> withTrait Resident
            ]
        )

instance RunMessage FrenziedMiner where
  runMessage msg e@(FrenziedMiner attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectEach
        (InvestigatorAt (locationWithEnemy attrs) <> not_ (InvestigatorWithId iid))
        (assignDamageTo (attrs.ability 1) 1)
      selectEach (AssetAt (locationWithEnemy attrs) <> withTrait Resident) \a -> dealAssetDirectDamage a (attrs.ability 1) 1
      pure e
    _ -> FrenziedMiner <$> liftRunMessage msg attrs
