module Arkham.Types.Asset.Cards.KeenEye
  ( keenEye
  , KeenEye(..)
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
import Arkham.Types.Restriction
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype KeenEye = KeenEye AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

keenEye :: AssetCard KeenEye
keenEye = asset KeenEye Cards.keenEye

instance HasActions KeenEye where
  getActions (KeenEye a) =
    [ restrictedAbility a idx OwnsThis (FastAbility $ ResourceCost 2)
    | idx <- [1, 2]
    ]

instance AssetRunner env => RunMessage env KeenEye where
  runMessage msg a@(KeenEye attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> a <$ push
      (CreateWindowModifierEffect
        EffectPhaseWindow
        (EffectModifiers $ toModifiers attrs [SkillModifier SkillIntellect 1])
        source
        (InvestigatorTarget iid)
      )
    UseCardAbility iid source _ 2 _ | isSource attrs source -> a <$ push
      (CreateWindowModifierEffect
        EffectPhaseWindow
        (EffectModifiers $ toModifiers attrs [SkillModifier SkillCombat 1])
        source
        (InvestigatorTarget iid)
      )
    _ -> KeenEye <$> runMessage msg attrs
