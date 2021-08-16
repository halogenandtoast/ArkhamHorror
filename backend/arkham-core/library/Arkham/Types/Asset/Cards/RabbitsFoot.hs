module Arkham.Types.Asset.Cards.RabbitsFoot where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Message
import Arkham.Types.Window

newtype RabbitsFoot = RabbitsFoot AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rabbitsFoot :: AssetCard RabbitsFoot
rabbitsFoot = accessory RabbitsFoot Cards.rabbitsFoot

instance HasModifiersFor env RabbitsFoot

instance HasAbilities env RabbitsFoot where
  getAbilities iid (AfterFailSkillTest who _) (RabbitsFoot a) | iid == who = pure
    [ mkAbility (toSource a) 1 (LegacyReactionAbility $ ExhaustCost (toTarget a))
    | ownedBy a iid
    ]
  getAbilities i window (RabbitsFoot x) = getAbilities i window x

instance AssetRunner env => RunMessage env RabbitsFoot where
  runMessage msg a@(RabbitsFoot attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (DrawCards iid 1 False)
    _ -> RabbitsFoot <$> runMessage msg attrs
