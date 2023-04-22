module Arkham.Asset.Cards.ToothOfEztli
  ( toothOfEztli
  , ToothOfEztli(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Source
import Arkham.Timing qualified as Timing

newtype ToothOfEztli = ToothOfEztli AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

toothOfEztli :: AssetCard ToothOfEztli
toothOfEztli = asset ToothOfEztli Cards.toothOfEztli

instance HasModifiersFor ToothOfEztli where
  getModifiersFor (InvestigatorTarget iid) (ToothOfEztli a)
    | controlledBy a iid = do
      mSkillTestSource <- getSkillTestSource
      case mSkillTestSource of
        Just (SkillTestSource _ _ (TreacherySource _) _) -> pure $ toModifiers
          a
          [SkillModifier SkillWillpower 1, SkillModifier SkillAgility 1]
        _ -> pure []
  getModifiersFor _ _ = pure []

instance HasAbilities ToothOfEztli where
  getAbilities (ToothOfEztli x) =
    [ restrictedAbility x 1 ControlsThis $ ReactionAbility
        (SkillTestResult
          Timing.After
          You
          (SkillTestOnTreachery AnyTreachery)
          (SuccessResult AnyValue)
        )
        (ExhaustCost $ toTarget x)
    ]

instance RunMessage ToothOfEztli where
  runMessage msg a@(ToothOfEztli attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      drawing <- drawCards iid attrs 1
      push drawing
      pure a
    _ -> ToothOfEztli <$> runMessage msg attrs
