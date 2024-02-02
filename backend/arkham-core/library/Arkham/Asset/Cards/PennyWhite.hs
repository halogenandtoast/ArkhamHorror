module Arkham.Asset.Cards.PennyWhite (
  pennyWhite,
  PennyWhite (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Discover
import Arkham.Matcher

newtype PennyWhite = PennyWhite AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

pennyWhite :: AssetCard PennyWhite
pennyWhite = allyWith PennyWhite Cards.pennyWhite (3, 2) (isStoryL .~ True)

instance HasModifiersFor PennyWhite where
  getModifiersFor (InvestigatorTarget iid) (PennyWhite a) | controlledBy a iid = do
    pure $ toModifiers a [SkillModifier #willpower 1]
  getModifiersFor _ _ = pure []

instance HasAbilities PennyWhite where
  getAbilities (PennyWhite x) =
    [ controlledAbility
        x
        1
        (CanDiscoverCluesAt YourLocation <> OnLocation LocationWithAnyClues)
        $ ReactionAbility
          (SkillTestResult #after You SkillTestFromRevelation $ SuccessResult AnyValue)
          (exhaust x)
    ]

instance RunMessage PennyWhite where
  runMessage msg a@(PennyWhite attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ discoverAtYourLocation iid (toSource attrs) 1
      pure a
    _ -> PennyWhite <$> runMessage msg attrs
