module Arkham.Types.Asset.Cards.HeirloomOfHyperborea where


import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Trait

newtype HeirloomOfHyperborea = HeirloomOfHyperborea AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

heirloomOfHyperborea :: AssetId -> HeirloomOfHyperborea
heirloomOfHyperborea uuid = HeirloomOfHyperborea
  $ (baseAttrs uuid "01012") { assetSlots = [AccessorySlot] }

instance HasModifiersFor env HeirloomOfHyperborea where
  getModifiersFor = noModifiersFor

reactionAbility :: AssetAttrs -> Ability
reactionAbility attrs = mkAbility (toSource attrs) 1 (FastAbility Free)

instance HasActions env HeirloomOfHyperborea where
  getActions iid (AfterPlayCard You traits) (HeirloomOfHyperborea a)
    | ownedBy a iid
    = pure
      [ ActivateCardAbilityAction iid (reactionAbility a)
      | Spell `elem` traits
      ]
  getActions i window (HeirloomOfHyperborea x) = getActions i window x

instance (AssetRunner env) => RunMessage env HeirloomOfHyperborea where
  runMessage msg a@(HeirloomOfHyperborea attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ unshiftMessage (DrawCards iid 1 False)
    _ -> HeirloomOfHyperborea <$> runMessage msg attrs
