module Arkham.Types.Asset.Cards.SpringfieldM19034
  ( springfieldM19034
  , SpringfieldM19034(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import qualified Arkham.Types.Asset.Uses as Resource
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Restriction
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Target

newtype SpringfieldM19034 = SpringfieldM19034 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

springfieldM19034 :: AssetCard SpringfieldM19034
springfieldM19034 = assetWith
  SpringfieldM19034
  Cards.springfieldM19034
  (slotsL .~ [HandSlot, HandSlot])

instance HasActions SpringfieldM19034 where
  getActions (SpringfieldM19034 a) =
    [ restrictedAbility a 1 OwnsThis $ ActionAbility
        (Just Action.Fight)
        (Costs [ActionCost 1, UseCost (toId a) Resource.Ammo 1])
    ]

instance (HasQueue env, HasModifiersFor env ()) => RunMessage env SpringfieldM19034 where
  runMessage msg a@(SpringfieldM19034 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> a <$ pushAll
      [ skillTestModifiers
        attrs
        (InvestigatorTarget iid)
        [DamageDealt 2, SkillModifier SkillCombat 3]
      , ChooseFightEnemyNotEngagedWithInvestigator iid source SkillCombat False
      ]
    _ -> SpringfieldM19034 <$> runMessage msg attrs
