module Arkham.Asset.Cards.FortyOneDerringer (
  FortyOneDerringer (..),
  fortyOneDerringer,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype FortyOneDerringer = FortyOneDerringer AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fortyOneDerringer :: AssetCard FortyOneDerringer
fortyOneDerringer = asset FortyOneDerringer Cards.fortyOneDerringer

instance HasAbilities FortyOneDerringer where
  getAbilities (FortyOneDerringer a) =
    [ restrictedAbility a 1 ControlsThis
        $ ActionAbility (Just Action.Fight)
        $ Costs [ActionCost 1, assetUseCost a Ammo 1]
    ]

instance RunMessage FortyOneDerringer where
  runMessage msg a@(FortyOneDerringer attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      pushAll
        [ skillTestModifier (toAbilitySource attrs 1) iid (SkillModifier #combat 2)
        , chooseFightEnemy iid (toAbilitySource attrs 1) #combat
        ]
      pure a
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n -> do
      pushWhen (n >= 2) $ skillTestModifier (toAbilitySource attrs 1) iid (DamageDealt 1)
      pure a
    _ -> FortyOneDerringer <$> runMessage msg attrs
