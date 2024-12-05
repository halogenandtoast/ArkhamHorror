module Arkham.Enemy.Cards.BoaConstrictor (
  boaConstrictor,
  BoaConstrictor (..),
  boaConstrictorEffect,
  BoaConstrictorEffect (..),
) where

import Arkham.Ability
import Arkham.Effect.Import
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers (ModifierType (..), modifiedWhen_)
import Arkham.Matcher
import Arkham.Phase

newtype BoaConstrictor = BoaConstrictor EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

boaConstrictor :: EnemyCard BoaConstrictor
boaConstrictor =
  enemy BoaConstrictor Cards.boaConstrictor (4, Static 4, 2) (1, 1)

instance HasAbilities BoaConstrictor where
  getAbilities (BoaConstrictor a) =
    extend a [mkAbility a 1 $ forced $ EnemyAttacks #after You AnyEnemyAttack (be a)]

instance RunMessage BoaConstrictor where
  runMessage msg e@(BoaConstrictor attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      createCardEffect Cards.boaConstrictor Nothing (attrs.ability 1) iid
      pure e
    _ -> BoaConstrictor <$> liftRunMessage msg attrs

newtype BoaConstrictorEffect = BoaConstrictorEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

boaConstrictorEffect :: EffectArgs -> BoaConstrictorEffect
boaConstrictorEffect = cardEffect BoaConstrictorEffect Cards.boaConstrictor

instance HasModifiersFor BoaConstrictorEffect where
  getModifiersFor (BoaConstrictorEffect a) = do
    phase <- getPhase
    modifiedWhen_ a (phase == UpkeepPhase) a.target [ControlledAssetsCannotReady]

instance RunMessage BoaConstrictorEffect where
  runMessage msg e@(BoaConstrictorEffect attrs) = runQueueT $ case msg of
    EndUpkeep -> disableReturn e
    _ -> BoaConstrictorEffect <$> liftRunMessage msg attrs
