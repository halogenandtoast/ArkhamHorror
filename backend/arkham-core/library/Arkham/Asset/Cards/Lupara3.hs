module Arkham.Asset.Cards.Lupara3 (lupara3, Lupara3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Fight
import Arkham.Prelude

newtype Metadata = Metadata {justPlayed :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype Lupara3 = Lupara3 (AssetAttrs `With` Metadata)
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lupara3 :: AssetCard Lupara3
lupara3 = asset (Lupara3 . (`with` Metadata True)) Cards.lupara3

instance HasAbilities Lupara3 where
  getAbilities (Lupara3 a) = [restrictedAbility a 1 ControlsThis $ fightAction $ assetUseCost a Ammo 1]

instance RunMessage Lupara3 where
  runMessage msg a@(Lupara3 (attrs `With` metadata)) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let n = if justPlayed metadata then 2 else 1
      let source = attrs.ability 1
      chooseFight <- toMessage <$> mkChooseFight iid source
      pushAll [skillTestModifiers source iid [DamageDealt n, SkillModifier #combat n], chooseFight]
      pure a
    EndTurn _ -> pure . Lupara3 $ attrs `with` Metadata False
    _ -> Lupara3 . (`with` metadata) <$> runMessage msg attrs
