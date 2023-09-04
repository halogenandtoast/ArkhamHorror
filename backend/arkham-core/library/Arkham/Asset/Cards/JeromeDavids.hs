module Arkham.Asset.Cards.JeromeDavids (
  jeromeDavids,
  JeromeDavids (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.SkillType

newtype JeromeDavids = JeromeDavids AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jeromeDavids :: AssetCard JeromeDavids
jeromeDavids =
  allyWith JeromeDavids Cards.jeromeDavids (1, 4) (isStoryL .~ True)

instance HasModifiersFor JeromeDavids where
  getModifiersFor (InvestigatorTarget iid) (JeromeDavids a) =
    pure [toModifier a (SkillModifier SkillIntellect 1) | controlledBy a iid]
  getModifiersFor _ _ = pure []

instance HasAbilities JeromeDavids where
  getAbilities (JeromeDavids a) = [restrictedAbility a 1 ControlsThis $ ReactionAbility undefined (exhaust a)]

instance RunMessage JeromeDavids where
  runMessage msg (JeromeDavids attrs) = JeromeDavids <$> runMessage msg attrs
