module Arkham.Types.Asset.Cards.DigDeep2
  ( DigDeep2(..)
  , digDeep2
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

newtype DigDeep2 = DigDeep2 AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

digDeep2 :: AssetCard DigDeep2
digDeep2 = asset DigDeep2 Cards.digDeep2

instance HasModifiersFor env DigDeep2 where
  getModifiersFor = noModifiersFor

ability :: Int -> AssetAttrs -> Ability
ability idx a = mkAbility (toSource a) idx (FastAbility $ ResourceCost 1)

instance HasActions env DigDeep2 where
  getActions iid (WhenSkillTest SkillWillpower) (DigDeep2 a) = do
    pure [ ActivateCardAbilityAction iid (ability 1 a) | ownedBy a iid ]
  getActions iid (WhenSkillTest SkillAgility) (DigDeep2 a) = do
    pure [ ActivateCardAbilityAction iid (ability 2 a) | ownedBy a iid ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env DigDeep2 where
  runMessage msg a@(DigDeep2 attrs) = case msg of
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
    _ -> DigDeep2 <$> runMessage msg attrs
