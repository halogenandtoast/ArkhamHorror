module Arkham.Asset.Cards.ColtVestPocket (
  coltVestPocket,
  ColtVestPocket (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher

newtype ColtVestPocket = ColtVestPocket AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

coltVestPocket :: AssetCard ColtVestPocket
coltVestPocket = asset ColtVestPocket Cards.coltVestPocket

instance HasAbilities ColtVestPocket where
  getAbilities (ColtVestPocket a) =
    [ restrictedAbility a 1 ControlsThis
        $ fightAction
        $ assetUseCost a Ammo 1
    , restrictedAbility a 2 ControlsThis $ ForcedAbility $ RoundEnds #when
    ]

instance RunMessage ColtVestPocket where
  runMessage msg a@(ColtVestPocket attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      pushAll
        [ skillTestModifiers attrs iid [SkillModifier #combat 1, DamageDealt 1]
        , chooseFightEnemy iid attrs #combat
        ]
      pure a
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      push $ toDiscardBy iid (toAbilitySource attrs 2) attrs
      pure a
    _ -> ColtVestPocket <$> runMessage msg attrs
