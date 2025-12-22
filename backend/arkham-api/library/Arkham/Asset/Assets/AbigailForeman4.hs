module Arkham.Asset.Assets.AbigailForeman4 (abigailForeman4) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.Window (getAbility)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement

newtype AbigailForeman4 = AbigailForeman4 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

abigailForeman4 :: AssetCard AbigailForeman4
abigailForeman4 = ally AbigailForeman4 Cards.abigailForeman4 (1, 2)

instance HasModifiersFor AbigailForeman4 where
  getModifiersFor (AbigailForeman4 a) = for_ a.controller \iid -> do
    modifySelect a (AssetAttachedToAsset (be a)) [AsIfUnderControlOf iid]

instance HasAbilities AbigailForeman4 where
  getAbilities (AbigailForeman4 a) =
    [ controlled a 1 criteria $ FastAbility Free
    , controlled_ a 2
        $ triggered
          ( ActivateAbility #after You
              $ PerformableAbility [IgnoreAllCosts]
              <> #action
              <> AssetAbility (AssetAttachedToAsset (be a))
          )
          (exhaust a)
    ]
   where
    criteria = case a.controller of
      Just iid -> exists $ #tome <> assetInPlayAreaOf iid <> not_ (AssetAttachedToAsset (be a))
      Nothing -> Never

instance RunMessage AbigailForeman4 where
  runMessage msg a@(AbigailForeman4 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      as <- select $ assetControlledBy iid <> #tome <> assetInPlayAreaOf iid
      mAttachedAsset <- selectOne $ AssetAttachedToAsset (be attrs)
      chooseOrRunOneM iid do
        targets as \x -> do
          place x $ AttachedToAsset (toId attrs) (Just $ InPlayArea iid)
          for_ mAttachedAsset (`place` InPlayArea iid)
          push $ RefillSlots iid []
      pure a
    UseCardAbility iid (isSource attrs -> True) 2 (getAbility -> (ab, ws)) _ -> do
      push $ UseAbility iid (ignoreAllCosts ab) ws
      pure a
    _ -> AbigailForeman4 <$> liftRunMessage msg attrs
