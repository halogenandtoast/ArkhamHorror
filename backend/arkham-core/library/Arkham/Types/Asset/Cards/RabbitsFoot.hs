module Arkham.Types.Asset.Cards.RabbitsFoot where


import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner

newtype RabbitsFoot = RabbitsFoot AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rabbitsFoot :: AssetId -> RabbitsFoot
rabbitsFoot uuid =
  RabbitsFoot $ (baseAttrs uuid "01075") { assetSlots = [AccessorySlot] }

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
