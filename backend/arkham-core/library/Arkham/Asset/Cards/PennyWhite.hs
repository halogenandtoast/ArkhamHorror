module Arkham.Asset.Cards.PennyWhite (
  pennyWhite,
  PennyWhite (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Timing qualified as Timing

newtype PennyWhite = PennyWhite AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pennyWhite :: AssetCard PennyWhite
pennyWhite =
  allyWith PennyWhite Cards.pennyWhite (3, 2) (isStoryL .~ True)

instance HasModifiersFor PennyWhite where
  getModifiersFor (InvestigatorTarget iid) (PennyWhite a) =
    pure [toModifier a (SkillModifier SkillWillpower 1) | controlledBy a iid]
  getModifiersFor _ _ = pure []

instance HasAbilities PennyWhite where
  getAbilities (PennyWhite x) =
    [ restrictedAbility
        x
        1
        (ControlsThis <> CanDiscoverCluesAt YourLocation <> OnLocation LocationWithAnyClues)
        $ ReactionAbility
          (SkillTestResult Timing.After You SkillTestFromRevelation $ SuccessResult AnyValue)
          (exhaust x)
    ]

instance RunMessage PennyWhite where
  runMessage msg a@(PennyWhite attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ InvestigatorDiscoverCluesAtTheirLocation iid (toSource attrs) 1 Nothing
      pure a
    _ -> PennyWhite <$> runMessage msg attrs
