module Arkham.Event.Events.Reliable1 (reliable1) where

import Arkham.Ability
import Arkham.Asset.Types (Field (..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Message.Lifted.Upgrade
import Arkham.Projection
import Arkham.Trait

newtype Reliable1 = Reliable1 EventAttrs
  deriving anyclass (IsEvent, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

reliable1 :: EventCard Reliable1
reliable1 = event Reliable1 Cards.reliable1

instance HasModifiersFor Reliable1 where
  getModifiersFor (Reliable1 a) = maybeModified_ a a.controller do
    AssetTarget aid <- hoistMaybe a.attachedTo
    owner <- MaybeT $ field AssetController aid
    guard $ owner == a.controller
    abilities <- lift getActiveAbilities
    let isAttachedTargetAbility = (== AssetSource aid) . abilitySource
    guard $ any (and . sequence [isAttachedTargetAbility, isTriggeredAbility]) abilities
    pure [AnySkillValue 1]

instance RunMessage Reliable1 where
  runMessage msg e@(Reliable1 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      assets <- getUpgradeTargets iid $ assetControlledBy iid <> AssetWithTrait Item
      chooseTargetM iid assets \asset -> place attrs $ AttachedToAsset asset Nothing
      pure e
    _ -> Reliable1 <$> liftRunMessage msg attrs
