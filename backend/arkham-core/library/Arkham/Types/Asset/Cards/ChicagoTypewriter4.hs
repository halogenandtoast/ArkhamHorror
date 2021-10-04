module Arkham.Types.Asset.Cards.ChicagoTypewriter4
  ( ChicagoTypewriter4(..)
  , chicagoTypewriter4
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Action qualified as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Target

newtype ChicagoTypewriter4 = ChicagoTypewriter4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chicagoTypewriter4 :: AssetCard ChicagoTypewriter4
chicagoTypewriter4 = assetWith
  ChicagoTypewriter4
  Cards.chicagoTypewriter4
  (slotsL .~ [HandSlot, HandSlot])

instance HasAbilities ChicagoTypewriter4 where
  getAbilities (ChicagoTypewriter4 a) =
    [ restrictedAbility a 1 OwnsThis $ ActionAbility (Just Action.Fight) $ Costs
        [ActionCost 1, AdditionalActionsCost, UseCost (toId a) Ammo 1]
    ]

getAbilitiesSpent :: Payment -> Int
getAbilitiesSpent (ActionPayment n) = n
getAbilitiesSpent (Payments ps) = sum $ map getAbilitiesSpent ps
getAbilitiesSpent _ = 0

instance AssetRunner env => RunMessage env ChicagoTypewriter4 where
  runMessage msg a@(ChicagoTypewriter4 attrs) = case msg of
    UseCardAbility iid source _ 1 payment | isSource attrs source -> do
      let actionsSpent = getAbilitiesSpent payment
      a <$ pushAll
        [ skillTestModifiers
          attrs
          (InvestigatorTarget iid)
          [DamageDealt 2, SkillModifier SkillCombat (2 * actionsSpent)]
        , ChooseFightEnemy iid source Nothing SkillCombat mempty False
        ]
    _ -> ChicagoTypewriter4 <$> runMessage msg attrs
