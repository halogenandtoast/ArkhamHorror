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
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rabbitsFoot :: AssetCard RabbitsFoot
rabbitsFoot = accessory RabbitsFoot Cards.rabbitsFoot

instance HasModifiersFor env RabbitsFoot where
  getModifiersFor = noModifiersFor

instance HasActions env RabbitsFoot where
  getActions iid (AfterFailSkillTest You _) (RabbitsFoot a) = pure
    [ ActivateCardAbilityAction
        iid
        (mkAbility (toSource a) 1 (ReactionAbility $ ExhaustCost (toTarget a)))
    | ownedBy a iid
    ]
  getActions i window (RabbitsFoot x) = getActions i window x

instance AssetRunner env => RunMessage env RabbitsFoot where
  runMessage msg a@(RabbitsFoot attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ unshiftMessage (DrawCards iid 1 False)
    _ -> RabbitsFoot <$> runMessage msg attrs
