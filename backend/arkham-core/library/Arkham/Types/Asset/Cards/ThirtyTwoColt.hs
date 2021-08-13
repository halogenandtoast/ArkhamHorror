module Arkham.Types.Asset.Cards.ThirtyTwoColt
  ( thirtyTwoColt
  , ThirtyTwoColt(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Uses
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Game.Helpers
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Restriction
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Target

newtype ThirtyTwoColt = ThirtyTwoColt AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thirtyTwoColt :: AssetCard ThirtyTwoColt
thirtyTwoColt =
  assetWith ThirtyTwoColt Cards.thirtyTwoColt (slotsL .~ [HandSlot])

instance HasActions ThirtyTwoColt where
  getActions (ThirtyTwoColt a) =
    [ restrictedAbility a 1 OwnsThis $ ActionAbility
        (Just Action.Fight)
        (Costs [ActionCost 1, UseCost (toId a) Ammo 1])
    ]

instance (HasQueue env, HasModifiersFor env ()) => RunMessage env ThirtyTwoColt where
  runMessage msg a@(ThirtyTwoColt attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> a <$ pushAll
      [ skillTestModifier attrs (InvestigatorTarget iid) (DamageDealt 1)
      , ChooseFightEnemy iid source SkillCombat mempty False
      ]
    _ -> ThirtyTwoColt <$> runMessage msg attrs
