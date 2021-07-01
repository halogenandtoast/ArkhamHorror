module Arkham.Types.Asset.Cards.ArcaneStudies2
  ( ArcaneStudies2(..)
  , arcaneStudies2
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Effect.Window
import Arkham.Types.EffectMetadata
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Window

newtype ArcaneStudies2 = ArcaneStudies2 AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arcaneStudies2 :: AssetCard ArcaneStudies2
arcaneStudies2 = asset ArcaneStudies2 Cards.arcaneStudies2

instance HasModifiersFor env ArcaneStudies2 where
  getModifiersFor = noModifiersFor

ability :: Int -> AssetAttrs -> Ability
ability idx a = mkAbility (toSource a) idx (FastAbility $ ResourceCost 1)

instance HasActions env ArcaneStudies2 where
  getActions iid (WhenSkillTest SkillWillpower) (ArcaneStudies2 a) =
    pure [ ActivateCardAbilityAction iid (ability 1 a) | ownedBy a iid ]
  getActions iid (WhenSkillTest SkillIntellect) (ArcaneStudies2 a) =
    pure [ ActivateCardAbilityAction iid (ability 2 a) | ownedBy a iid ]
  getActions _ _ _ = pure []

instance AssetRunner env => RunMessage env ArcaneStudies2 where
  runMessage msg a@(ArcaneStudies2 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ unshiftMessage
        (CreateWindowModifierEffect
          EffectSkillTestWindow
          (EffectModifiers $ toModifiers attrs [SkillModifier SkillWillpower 1])
          source
          (InvestigatorTarget iid)
        )
    UseCardAbility iid source _ 2 _ | isSource attrs source ->
      a <$ unshiftMessage
        (CreateWindowModifierEffect
          EffectSkillTestWindow
          (EffectModifiers $ toModifiers attrs [SkillModifier SkillIntellect 1])
          source
          (InvestigatorTarget iid)
        )
    _ -> ArcaneStudies2 <$> runMessage msg attrs
