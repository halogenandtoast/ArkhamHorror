module Arkham.Asset.Cards.ChicagoTypewriter4 (
  ChicagoTypewriter4 (..),
  chicagoTypewriter4,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.SkillType

newtype ChicagoTypewriter4 = ChicagoTypewriter4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chicagoTypewriter4 :: AssetCard ChicagoTypewriter4
chicagoTypewriter4 = asset ChicagoTypewriter4 Cards.chicagoTypewriter4

instance HasAbilities ChicagoTypewriter4 where
  getAbilities (ChicagoTypewriter4 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ActionAbility ([Action.Fight])
        $ Costs
          [ActionCost 1, AdditionalActionsCost, UseCost (AssetWithId $ toId a) Ammo 1]
    ]

getAbilitiesSpent :: Payment -> Int
getAbilitiesSpent (ActionPayment n) = n
getAbilitiesSpent (Payments ps) = sum $ map getAbilitiesSpent ps
getAbilitiesSpent _ = 0

instance RunMessage ChicagoTypewriter4 where
  runMessage msg a@(ChicagoTypewriter4 attrs) = case msg of
    UseCardAbility iid source 1 _ payment | isSource attrs source -> do
      let actionsSpent = getAbilitiesSpent payment
      a
        <$ pushAll
          [ skillTestModifiers
              attrs
              (InvestigatorTarget iid)
              [DamageDealt 2, SkillModifier SkillCombat (2 * actionsSpent)]
          , ChooseFightEnemy iid source Nothing SkillCombat mempty False
          ]
    _ -> ChicagoTypewriter4 <$> runMessage msg attrs
