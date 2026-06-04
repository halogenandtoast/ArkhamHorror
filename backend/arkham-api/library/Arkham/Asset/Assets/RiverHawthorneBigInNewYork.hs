module Arkham.Asset.Assets.RiverHawthorneBigInNewYork (riverHawthorneBigInNewYork) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (AssetDefeated)
import Arkham.Card
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers (Resident (..), codex, decreaseRelationshipLevel)
import Arkham.Message.Lifted.Choose

newtype RiverHawthorneBigInNewYork = RiverHawthorneBigInNewYork AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riverHawthorneBigInNewYork :: AssetCard RiverHawthorneBigInNewYork
riverHawthorneBigInNewYork = assetWith RiverHawthorneBigInNewYork Cards.riverHawthorneBigInNewYork $ (healthL ?~ 2) . (sanityL ?~ 2)

instance HasModifiersFor RiverHawthorneBigInNewYork where
  getModifiersFor (RiverHawthorneBigInNewYork a) = controllerGets a [SkillModifier #willpower 1]

instance HasAbilities RiverHawthorneBigInNewYork where
  getAbilities (RiverHawthorneBigInNewYork a) =
    [ mkAbility a 99 $ forced $ AssetDefeated #when ByAny (be a)
    ,groupLimit PerGame $ skillTestAbility $ restricted a 1 OnSameLocation parleyAction_]

instance RunMessage RiverHawthorneBigInNewYork where
  runMessage msg a@(RiverHawthorneBigInNewYork attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseBeginSkillTest sid iid (attrs.ability 1) attrs [#willpower, #intellect] (Fixed 3)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      codex iid (attrs.ability 1) 5
      pure a
    UseCardAbility _ (isSource attrs -> True) 99 ws _ -> do
      cancelWindowBatch ws
      removeFromGame attrs
      push $ SetAsideCards [toCard attrs]
      decreaseRelationshipLevel RiverHawthorne 1
      pure a
    _ -> RiverHawthorneBigInNewYork <$> liftRunMessage msg attrs
