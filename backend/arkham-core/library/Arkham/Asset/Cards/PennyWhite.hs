module Arkham.Asset.Cards.PennyWhite (
  pennyWhite,
  PennyWhite (..),
)
where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.SkillType

newtype PennyWhite = PennyWhite AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pennyWhite :: AssetCard PennyWhite
pennyWhite =
  allyWith PennyWhite Cards.pennyWhite (3, 2) (isStoryL .~ True)

instance HasModifiersFor PennyWhite where
  getModifiersFor (InvestigatorTarget iid) (PennyWhite a) =
    pure [toModifier a (SkillModifier SkillWillpower 1) | controlledBy a iid]
  getModifiersFor _ _ = pure []

instance RunMessage PennyWhite where
  runMessage msg (PennyWhite attrs) = PennyWhite <$> runMessage msg attrs
