module Arkham.Asset.Assets.RiverHawthorneBigInNewYork (riverHawthorneBigInNewYork) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (AssetDefeated)
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.SkillTest.Base

newtype RiverHawthorneBigInNewYork = RiverHawthorneBigInNewYork AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riverHawthorneBigInNewYork :: AssetCard RiverHawthorneBigInNewYork
riverHawthorneBigInNewYork =
  assetWith RiverHawthorneBigInNewYork Cards.riverHawthorneBigInNewYork
    $ (healthL ?~ 2)
    . (sanityL ?~ 2)

instance HasModifiersFor RiverHawthorneBigInNewYork where
  getModifiersFor (RiverHawthorneBigInNewYork a) = controllerGets a [SkillModifier #willpower 1]

instance HasAbilities RiverHawthorneBigInNewYork where
  getAbilities (RiverHawthorneBigInNewYork a) =
    [ skillTestAbility $ restricted a 1 (OnSameLocation <> youCanTriggerCodex 5) parleyAction_
    , mkAbility a 2 $ forced $ AssetDefeated #when ByAny (be a)
    ]

instance RunMessage RiverHawthorneBigInNewYork where
  runMessage msg a@(RiverHawthorneBigInNewYork attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseBeginSkillTestEdit sid iid (attrs.ability 1) attrs [#willpower, #intellect] (Fixed 3) \st -> st {skillTestAction = Just #parley}
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      codex iid (attrs.ability 1) 5
      pure a
    UseCardAbility _ (isSource attrs -> True) 2 ws _ -> do
      cancelWindowBatch ws
      removeFromGame attrs
      setCardAside attrs
      decreaseRelationshipLevel RiverHawthorne 1
      pure a
    _ -> RiverHawthorneBigInNewYork <$> liftRunMessage msg attrs
