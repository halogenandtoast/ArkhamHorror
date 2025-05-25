module Arkham.Asset.Assets.ArcaneInsight4 (arcaneInsight4) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Location
import Arkham.Helpers.Modifiers
import Arkham.Matcher hiding (DuringTurn)

newtype ArcaneInsight4 = ArcaneInsight4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arcaneInsight4 :: AssetCard ArcaneInsight4
arcaneInsight4 = asset ArcaneInsight4 Cards.arcaneInsight4

instance HasAbilities ArcaneInsight4 where
  getAbilities (ArcaneInsight4 a) =
    [playerLimit PerTurn $ controlled a 1 (DuringTurn Anyone) $ FastAbility $ assetUseCost a Charge 1]

instance RunMessage ArcaneInsight4 where
  runMessage msg a@(ArcaneInsight4 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withLocationOf iid \lid -> currentTurnModifier (attrs.ability 1) lid (ShroudModifier (-2))
      pure a
    _ -> ArcaneInsight4 <$> liftRunMessage msg attrs
