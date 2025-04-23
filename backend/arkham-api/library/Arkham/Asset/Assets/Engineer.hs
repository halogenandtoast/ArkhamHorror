module Arkham.Asset.Assets.Engineer (engineer) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Location (placementLocation, withLocationOf)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement

newtype Engineer = Engineer AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

engineer :: AssetCard Engineer
engineer = assetWith Engineer Cards.engineer ((healthL ?~ 2) . (sanityL ?~ 2))

instance HasAbilities Engineer where
  getAbilities (Engineer a) =
    [ skillTestAbility $ restricted a 1 (OnSameLocation <> thisExists a AssetReady) parleyAction_
    , restricted
        a
        2
        (criteria <> thisExists a (oneOf [AssetControlledBy You, not_ (AssetControlledBy Anyone)]))
        $ forced
        $ AssetWouldBeDiscarded #when (be a)
    ]
   where
    criteria = if toResultDefault True a.meta then NoRestriction else Never

instance RunMessage Engineer where
  runMessage msg a@(Engineer attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseOneM iid do
        for_ [#willpower, #intellect] \kind ->
          skillLabeled kind $ beginSkillTest sid iid (attrs.ability 1) attrs kind (Fixed 4)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      case attrs.controller of
        Just iid' | iid' == iid -> discoverAtYourLocation NotInvestigate iid (attrs.ability 1) 1
        _ -> takeControlOfAsset iid attrs
      pure a
    RemoveLocation lid -> do
      mLid <- placementLocation attrs.placement
      if Just lid == mLid
        then pure $ overAttrs (setMeta False) a
        else pure a
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      traverse_ cancelBatch =<< getCurrentBatchId
      healAllDamageAndHorror (attrs.ability 2) attrs
      for_ attrs.controller \iid' -> do
        withLocationOf iid' (place attrs)
      exhaustThis attrs
      pure a
    _ -> Engineer <$> liftRunMessage msg attrs
