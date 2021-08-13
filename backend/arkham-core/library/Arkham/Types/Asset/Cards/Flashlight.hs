module Arkham.Types.Asset.Cards.Flashlight
  ( Flashlight(..)
  , flashlight
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
import Arkham.Types.Target

newtype Flashlight = Flashlight AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

flashlight :: AssetCard Flashlight
flashlight = hand Flashlight Cards.flashlight

instance HasActions Flashlight where
  getActions (Flashlight x) =
    [ restrictedAbility x 1 OwnsThis $ ActionAbility
        (Just Action.Investigate)
        (Costs [ActionCost 1, UseCost (toId x) Supply 1])
    ]

instance (AssetRunner env) => RunMessage env Flashlight where
  runMessage msg a@(Flashlight attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      lid <- getId iid
      a <$ pushAll
        [ skillTestModifier attrs (LocationTarget lid) (ShroudModifier (-2))
        , Investigate iid lid source SkillIntellect False
        ]
    _ -> Flashlight <$> runMessage msg attrs
