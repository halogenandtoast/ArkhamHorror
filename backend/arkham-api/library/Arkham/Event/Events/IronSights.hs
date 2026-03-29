module Arkham.Event.Events.IronSights (ironSights) where

import Arkham.Ability
import Arkham.Effect.Builder
import Arkham.Effect.Window
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Upgrade
import Arkham.Modifier

newtype IronSights = IronSights EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ironSights :: EventCard IronSights
ironSights = event IronSights Cards.ironSights

instance HasAbilities IronSights where
  getAbilities (IronSights a) =
    case a.attachedTo.asset of
      Just _ -> [noAOO $ controlled_ a 1 $ actionAbilityWithCost (exhaust a)]
      _ -> []

instance RunMessage IronSights where
  runMessage msg e@(IronSights attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      assets <- getUpgradeTargets iid $ assetControlledBy iid <> #firearm
      chooseTargetM iid assets \asset -> place attrs $ AttachedToAsset asset Nothing
      pure e
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      for_ attrs.attachedTo.asset \asset -> do
        withSource (attrs.ability 1) $ effect iid do
          during $ EffectSkillTestMatchingWindow $ WhileAttacking <> SkillTestOnAsset (AssetWithId asset)
          removeOn $ #turn iid
          apply $ AnySkillValue 3
      pure e
    _ -> IronSights <$> liftRunMessage msg attrs
