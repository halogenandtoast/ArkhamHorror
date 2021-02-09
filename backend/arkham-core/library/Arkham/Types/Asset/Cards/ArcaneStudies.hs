module Arkham.Types.Asset.Cards.ArcaneStudies
  ( ArcaneStudies(..)
  , arcaneStudies
  )
where


import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner

newtype ArcaneStudies = ArcaneStudies AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arcaneStudies :: AssetId -> ArcaneStudies
arcaneStudies uuid = ArcaneStudies $ baseAttrs uuid "01062"

instance HasModifiersFor env ArcaneStudies where
  getModifiersFor = noModifiersFor

ability :: Int -> AssetAttrs -> Ability
ability idx a = mkAbility (toSource a) idx (FastAbility $ ResourceCost 1)

instance HasActions env ArcaneStudies where
  getActions iid (WhenSkillTest SkillWillpower) (ArcaneStudies a) =
    pure [ ActivateCardAbilityAction iid (ability 1 a) | ownedBy a iid ]
  getActions iid (WhenSkillTest SkillIntellect) (ArcaneStudies a) =
    pure [ ActivateCardAbilityAction iid (ability 2 a) | ownedBy a iid ]
  getActions _ _ _ = pure []

instance AssetRunner env => RunMessage env ArcaneStudies where
  runMessage msg a@(ArcaneStudies attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ unshiftMessage
        (CreateWindowModifierEffect EffectSkillTestWindow
          (EffectModifiers [toModifier attrs $ SkillModifier SkillWillpower 1])
          source
          (InvestigatorTarget iid)
        )
    UseCardAbility iid source _ 2 _ | isSource attrs source ->
      a <$ unshiftMessage
        (CreateWindowModifierEffect EffectSkillTestWindow
          (EffectModifiers [toModifier attrs $ SkillModifier SkillIntellect 1])
          source
          (InvestigatorTarget iid)
        )
    _ -> ArcaneStudies <$> runMessage msg attrs
