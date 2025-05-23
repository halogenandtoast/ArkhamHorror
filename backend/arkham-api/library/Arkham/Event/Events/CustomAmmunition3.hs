module Arkham.Event.Events.CustomAmmunition3 (customAmmunition3) where

import Arkham.Asset.Uses
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest
import Arkham.Matcher
import Arkham.Message.Lifted.Upgrade
import Arkham.Trait

newtype CustomAmmunition3 = CustomAmmunition3 EventAttrs
  deriving anyclass (IsEvent, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

customAmmunition3 :: EventCard CustomAmmunition3
customAmmunition3 = event CustomAmmunition3 Cards.customAmmunition3

instance HasModifiersFor CustomAmmunition3 where
  getModifiersFor (CustomAmmunition3 a) = maybeModified_ a a.controller do
    aid <- MaybeT $ ((.asset) =<<) <$> getSkillTestSource
    liftGuardM $ a.controller <=~> HasMatchingAsset (AssetWithId aid)
    guard $ maybe False (isTarget aid) a.attachedTo
    liftGuardM $ isFightWith $ EnemyWithTrait Monster
    pure [DamageDealt 1]

instance RunMessage CustomAmmunition3 where
  runMessage msg e@(CustomAmmunition3 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      assets <-
        getUpgradeTargets iid
          $ AssetControlledBy (affectsOthers $ colocatedWith iid)
          <> #firearm
          <> not_ (AssetWithAttachedEvent $ EventCardMatch $ cardIs Cards.customAmmunition3)

      chooseTargetM iid assets \asset -> do
        place attrs (AttachedToAsset asset Nothing)
        addUses attrs asset Ammo 2
      pure e
    _ -> CustomAmmunition3 <$> liftRunMessage msg attrs
