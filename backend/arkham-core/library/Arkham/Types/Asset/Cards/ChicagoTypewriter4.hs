module Arkham.Types.Asset.Cards.ChicagoTypewriter4
  ( ChicagoTypewriter4(..)
  , chicagoTypewriter4
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Restriction
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

instance HasActions ChicagoTypewriter4 where
  getActions (ChicagoTypewriter4 a) =
    [ restrictedAbility a 1 OwnsThis $ ActionAbility
        (Just Action.Fight)
        (Costs [ActionCost 1, AdditionalActionsCost, UseCost (toId a) Ammo 1])
    ]

getActionsSpent :: Payment -> Int
getActionsSpent (ActionPayment n) = n
getActionsSpent (Payments ps) = sum $ map getActionsSpent ps
getActionsSpent _ = 0

instance (AssetRunner env) => RunMessage env ChicagoTypewriter4 where
  runMessage msg a@(ChicagoTypewriter4 attrs) = case msg of
    UseCardAbility iid source _ 1 payment | isSource attrs source -> do
      let actionsSpent = getActionsSpent payment
      a <$ pushAll
        [ skillTestModifiers
          attrs
          (InvestigatorTarget iid)
          [DamageDealt 2, SkillModifier SkillCombat (2 * actionsSpent)]
        , ChooseFightEnemy iid source SkillCombat mempty False
        ]
    _ -> ChicagoTypewriter4 <$> runMessage msg attrs
