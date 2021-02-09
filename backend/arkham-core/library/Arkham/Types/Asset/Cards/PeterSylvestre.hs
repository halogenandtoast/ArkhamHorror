module Arkham.Types.Asset.Cards.PeterSylvestre
  ( PeterSylvestre(..)
  , peterSylvestre
  ) where


import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner

newtype PeterSylvestre = PeterSylvestre AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

peterSylvestre :: AssetId -> PeterSylvestre
peterSylvestre uuid = PeterSylvestre $ (baseAttrs uuid "02033")
  { assetSlots = [AllySlot]
  , assetHealth = Just 1
  , assetSanity = Just 2
  }

instance HasModifiersFor env PeterSylvestre where
  getModifiersFor _ (InvestigatorTarget iid) (PeterSylvestre a) =
    pure [ toModifier a (SkillModifier SkillAgility 1) | ownedBy a iid ]
  getModifiersFor _ _ _ = pure []

ability :: AssetAttrs -> Ability
ability attrs = mkAbility (toSource attrs) 1 (ReactionAbility Free)

instance HasActions env PeterSylvestre where
  getActions iid (AfterEndTurn You) (PeterSylvestre a) | ownedBy a iid =
    pure [ ActivateCardAbilityAction iid (ability a) | assetSanityDamage a > 0 ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env PeterSylvestre where
  runMessage msg (PeterSylvestre attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      pure $ PeterSylvestre $ attrs & sanityDamageL -~ 1
    _ -> PeterSylvestre <$> runMessage msg attrs
