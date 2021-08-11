module Arkham.Types.Asset.Cards.RabbitsFoot where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Message
import Arkham.Types.Restriction
import qualified Arkham.Types.Timing as Timing

newtype RabbitsFoot = RabbitsFoot AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rabbitsFoot :: AssetCard RabbitsFoot
rabbitsFoot = accessory RabbitsFoot Cards.rabbitsFoot

instance HasModifiersFor env RabbitsFoot

instance HasActions RabbitsFoot where
  getActions (RabbitsFoot a) =
    [ restrictedAbility a 1 OwnsThis $ ReactionAbility
        (SkillTestResult Timing.After You AnySkillTest (FailureResult AnyValue))
        ExhaustThis
    ]

instance AssetRunner env => RunMessage env RabbitsFoot where
  runMessage msg a@(RabbitsFoot attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (DrawCards iid 1 False)
    _ -> RabbitsFoot <$> runMessage msg attrs
