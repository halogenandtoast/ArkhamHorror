module Arkham.Types.Asset.Cards.HigherEducation
  ( higherEducation
  , HigherEducation(..)
  ) where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Effect.Window
import Arkham.Types.EffectMetadata
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Window
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner

newtype HigherEducation = HigherEducation AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

higherEducation :: AssetId -> HigherEducation
higherEducation uuid = HigherEducation $ baseAttrs uuid "60211"

instance HasList HandCard env InvestigatorId => HasActions env HigherEducation where
  getActions iid (WhenSkillTest SkillWillpower) (HigherEducation a)
    | ownedBy a iid = do
      active <- (>= 5) . length <$> getHandOf iid
      pure
        [ ActivateCardAbilityAction
            iid
            (mkAbility (toSource a) 1 (FastAbility $ ResourceCost 1))
        | active
        ]
  getActions iid (WhenSkillTest SkillIntellect) (HigherEducation a)
    | ownedBy a iid = do
      active <- (>= 5) . length <$> getHandOf iid
      pure
        [ ActivateCardAbilityAction
            iid
            (mkAbility (toSource a) 2 (FastAbility $ ResourceCost 1))
        | active
        ]
  getActions _ _ _ = pure []

instance HasModifiersFor env HigherEducation where
  getModifiersFor = noModifiersFor

instance AssetRunner env => RunMessage env HigherEducation where
  runMessage msg a@(HigherEducation attrs@AssetAttrs {..}) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ unshiftMessages
        [ SpendResources iid 1
        , CreateWindowModifierEffect EffectSkillTestWindow
          (EffectModifiers $ toModifiers attrs [SkillModifier SkillWillpower 1])
          source
          (InvestigatorTarget iid)
        ]
    UseCardAbility iid source _ 2 _ | isSource attrs source ->
      a <$ unshiftMessages
        [ SpendResources iid 1
        , CreateWindowModifierEffect EffectSkillTestWindow
          (EffectModifiers $ toModifiers attrs [SkillModifier SkillIntellect 1])
          source
          (InvestigatorTarget iid)
        ]
    _ -> HigherEducation <$> runMessage msg attrs
