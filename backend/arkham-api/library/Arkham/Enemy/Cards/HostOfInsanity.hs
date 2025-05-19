module Arkham.Enemy.Cards.HostOfInsanity (hostOfInsanity) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Ability
import Arkham.Matcher

newtype HostOfInsanity = HostOfInsanity EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hostOfInsanity :: EnemyCard HostOfInsanity
hostOfInsanity = enemy HostOfInsanity Cards.hostOfInsanity (4, Static 4, 4) (1, 1)

instance HasModifiersFor HostOfInsanity where
  getModifiersFor (HostOfInsanity a) = do
    healthModifier <- perPlayer 2
    modifySelf a [HealthModifier healthModifier]

instance HasAbilities HostOfInsanity where
  getAbilities (HostOfInsanity a) =
    extend1 a
      $ restricted a 1 (youExist $ ControlsAsset $ assetIs Assets.claspOfBlackOnyx) parleyAction_

instance RunMessage HostOfInsanity where
  runMessage msg e@(HostOfInsanity attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      automaticallyEvadeEnemy iid attrs
      pure e
    _ -> HostOfInsanity <$> liftRunMessage msg attrs
