module Arkham.Asset.Assets.JudithParkTheMuscle (judithParkTheMuscle) where

import Arkham.Ability
import Arkham.Action.Additional
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (AssetDefeated)
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Helpers.Modifiers
import Arkham.Matcher

newtype JudithParkTheMuscle = JudithParkTheMuscle AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

judithParkTheMuscle :: AssetCard JudithParkTheMuscle
judithParkTheMuscle = assetWith JudithParkTheMuscle Cards.judithParkTheMuscle $ (healthL ?~ 3) . (sanityL ?~ 2)

instance HasModifiersFor JudithParkTheMuscle where
  getModifiersFor (JudithParkTheMuscle a) =
    controllerGets
      a
      [ GiveAdditionalAction
          $ AdditionalAction "Judith Park" (toSource a)
          $ ActionRestrictedAdditionalAction #fight
      ]

instance HasAbilities JudithParkTheMuscle where
  getAbilities (JudithParkTheMuscle a) =
    let n = toResultDefault 1 a.meta
     in [ restricted a 1 (OnSameLocation <> youCanTriggerCodex 7) $ parleyAction (ResourceCost n)
        , mkAbility a 2 $ forced $ AssetDefeated #when ByAny (be a)
        ]

instance RunMessage JudithParkTheMuscle where
  runMessage msg a@(JudithParkTheMuscle attrs) = runQueueT $ case msg of
    PlaceAsset aid _ | aid == toId attrs -> do
      n <- dayNumber <$> getCampaignDay
      JudithParkTheMuscle <$> liftRunMessage msg (attrs & setMeta n)
    TakeControlOfAsset _ aid | aid == toId attrs -> do
      n <- dayNumber <$> getCampaignDay
      JudithParkTheMuscle <$> liftRunMessage msg (attrs & setMeta n)
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      codex iid (attrs.ability 1) 7
      pure a
    UseCardAbility _ (isSource attrs -> True) 2 ws _ -> do
      cancelWindowBatch ws
      removeFromGame attrs
      setCardAside attrs
      decreaseRelationshipLevel JudithPark 1
      pure a
    _ -> JudithParkTheMuscle <$> liftRunMessage msg attrs
