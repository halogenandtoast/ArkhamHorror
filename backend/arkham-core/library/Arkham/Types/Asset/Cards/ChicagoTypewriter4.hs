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
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Target
import Arkham.Types.Window

newtype ChicagoTypewriter4 = ChicagoTypewriter4 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chicagoTypewriter4 :: AssetCard ChicagoTypewriter4
chicagoTypewriter4 = assetWith
  ChicagoTypewriter4
  Cards.chicagoTypewriter4
  (slotsL .~ [HandSlot, HandSlot])

instance HasModifiersFor env ChicagoTypewriter4

instance HasAbilities env ChicagoTypewriter4 where
  getAbilities iid NonFast (ChicagoTypewriter4 a) | ownedBy a iid = pure
    [ mkAbility a 1 $ ActionAbility
        (Just Action.Fight)
        (Costs [ActionCost 1, AdditionalActionsCost, UseCost (toId a) Ammo 1])
    ]
  getAbilities _ _ _ = pure []

getAbilitiesSpent :: Payment -> Int
getAbilitiesSpent (ActionPayment n) = n
getAbilitiesSpent (Payments ps) = sum $ map getAbilitiesSpent ps
getAbilitiesSpent _ = 0

instance (AssetRunner env) => RunMessage env ChicagoTypewriter4 where
  runMessage msg a@(ChicagoTypewriter4 attrs) = case msg of
    UseCardAbility iid source _ 1 payment | isSource attrs source -> do
      let actionsSpent = getAbilitiesSpent payment
      a <$ pushAll
        [ skillTestModifiers
          attrs
          (InvestigatorTarget iid)
          [DamageDealt 2, SkillModifier SkillCombat (2 * actionsSpent)]
        , ChooseFightEnemy iid source SkillCombat mempty False
        ]
    _ -> ChicagoTypewriter4 <$> runMessage msg attrs
