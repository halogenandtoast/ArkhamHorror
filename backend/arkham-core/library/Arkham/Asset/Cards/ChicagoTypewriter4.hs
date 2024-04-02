module Arkham.Asset.Cards.ChicagoTypewriter4 (ChicagoTypewriter4 (..), chicagoTypewriter4) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Fight
import Arkham.Prelude

newtype ChicagoTypewriter4 = ChicagoTypewriter4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chicagoTypewriter4 :: AssetCard ChicagoTypewriter4
chicagoTypewriter4 = asset ChicagoTypewriter4 Cards.chicagoTypewriter4

instance HasAbilities ChicagoTypewriter4 where
  getAbilities (ChicagoTypewriter4 a) =
    [ restrictedAbility a 1 ControlsThis $ fightAction $ AdditionalActionsCost <> assetUseCost a Ammo 1
    ]

getActionsSpent :: Payment -> Int
getActionsSpent (ActionPayment n) = n
getActionsSpent (Payments ps) = sum $ map getActionsSpent ps
getActionsSpent _ = 0

instance RunMessage ChicagoTypewriter4 where
  runMessage msg a@(ChicagoTypewriter4 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (getActionsSpent -> actionsSpent) -> do
      chooseFight <- toMessage <$> mkChooseFight iid (attrs.ability 1)
      pushAll
        [ skillTestModifiers attrs iid [DamageDealt 2, SkillModifier #combat (2 * actionsSpent)]
        , chooseFight
        ]
      pure a
    _ -> ChicagoTypewriter4 <$> runMessage msg attrs
