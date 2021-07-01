module Arkham.Types.Asset.Cards.DigDeep
  ( DigDeep(..)
  , digDeep
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

newtype DigDeep = DigDeep AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

digDeep :: AssetCard DigDeep
digDeep = asset DigDeep Cards.digDeep

instance HasModifiersFor env DigDeep where
  getModifiersFor = noModifiersFor

ability :: Int -> AssetAttrs -> Ability
ability idx a = mkAbility (toSource a) idx (FastAbility $ ResourceCost 1)

instance HasActions env DigDeep where
  getActions iid (WhenSkillTest SkillWillpower) (DigDeep a) =
    pure [ ActivateCardAbilityAction iid (ability 1 a) | ownedBy a iid ]
  getActions iid (WhenSkillTest SkillAgility) (DigDeep a) =
    pure [ ActivateCardAbilityAction iid (ability 2 a) | ownedBy a iid ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env DigDeep where
  runMessage msg a@(DigDeep attrs) = case msg of
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
          (EffectModifiers $ toModifiers attrs [SkillModifier SkillAgility 1])
          source
          (InvestigatorTarget iid)
        )
    _ -> DigDeep <$> runMessage msg attrs
