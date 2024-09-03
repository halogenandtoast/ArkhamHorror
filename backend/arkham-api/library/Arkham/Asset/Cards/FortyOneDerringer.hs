module Arkham.Asset.Cards.FortyOneDerringer (FortyOneDerringer (..), fortyOneDerringer) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Fight
import Arkham.Prelude

newtype FortyOneDerringer = FortyOneDerringer AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fortyOneDerringer :: AssetCard FortyOneDerringer
fortyOneDerringer = asset FortyOneDerringer Cards.fortyOneDerringer

instance HasAbilities FortyOneDerringer where
  getAbilities (FortyOneDerringer a) =
    [restrictedAbility a 1 ControlsThis $ fightAction (assetUseCost a Ammo 1)]

instance RunMessage FortyOneDerringer where
  runMessage msg a@(FortyOneDerringer attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      chooseFight <- toMessage <$> mkChooseFight sid iid source
      pushAll [skillTestModifier sid source iid (SkillModifier #combat 2), chooseFight]
      pure a
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n -> do
      withSkillTest \sid ->
        pushWhen (n >= 2) $ skillTestModifier sid (attrs.ability 1) iid (DamageDealt 1)
      pure a
    _ -> FortyOneDerringer <$> runMessage msg attrs
