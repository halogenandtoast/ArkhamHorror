module Arkham.Types.Asset.Cards.JennysTwin45s
  ( JennysTwin45s(..)
  , jennysTwin45s
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

newtype JennysTwin45s = JennysTwin45s AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jennysTwin45s :: AssetCard JennysTwin45s
jennysTwin45s =
  assetWith JennysTwin45s Cards.jennysTwin45s (slotsL .~ [HandSlot, HandSlot])

instance HasActions JennysTwin45s where
  getActions (JennysTwin45s a) =
    [ restrictedAbility a 1 OwnsThis $ ActionAbility
        (Just Action.Fight)
        (Costs [ActionCost 1, UseCost (toId a) Ammo 1])
    ]

instance AssetRunner env => RunMessage env JennysTwin45s where
  runMessage msg a@(JennysTwin45s attrs) = case msg of
    InvestigatorPlayDynamicAsset _ aid _ _ n | aid == assetId attrs ->
      JennysTwin45s <$> runMessage msg (attrs & usesL .~ Uses Ammo n)
    UseCardAbility iid source _ 1 _ | isSource attrs source -> a <$ pushAll
      [ skillTestModifiers
        attrs
        (InvestigatorTarget iid)
        [DamageDealt 1, SkillModifier SkillCombat 2]
      , ChooseFightEnemy iid source SkillCombat mempty False
      ]
    _ -> JennysTwin45s <$> runMessage msg attrs
