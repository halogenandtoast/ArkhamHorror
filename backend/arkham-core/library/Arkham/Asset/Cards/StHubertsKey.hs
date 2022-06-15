module Arkham.Asset.Cards.StHubertsKey
  ( stHubertsKey
  , StHubertsKey(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Attrs
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding (InvestigatorDefeated)
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.Modifier
import Arkham.SkillType
import Arkham.Target
import Arkham.Timing qualified as Timing

newtype StHubertsKey = StHubertsKey AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stHubertsKey :: AssetCard StHubertsKey
stHubertsKey = asset StHubertsKey Cards.stHubertsKey

instance HasModifiersFor StHubertsKey where
  getModifiersFor _ (InvestigatorTarget iid) (StHubertsKey a)
    | Just iid == assetController a = pure $ toModifiers
      a
      [ SkillModifier SkillWillpower 1
      , SkillModifier SkillIntellect 1
      , SanityModifier (-2)
      ]
  getModifiersFor _ _ _ = pure []

instance HasAbilities StHubertsKey where
  getAbilities (StHubertsKey a) =
    [ restrictedAbility a 1 OwnsThis
        $ ReactionAbility
            (InvestigatorDefeated Timing.When AnySource ByHorror You)
            Free
    ]

instance RunMessage StHubertsKey where
  runMessage msg (StHubertsKey attrs) = StHubertsKey <$> runMessage msg attrs
