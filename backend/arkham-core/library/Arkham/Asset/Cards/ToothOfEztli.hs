module Arkham.Asset.Cards.ToothOfEztli
  ( toothOfEztli
  , ToothOfEztli(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.Modifier
import Arkham.SkillType
import Arkham.Source
import Arkham.Target
import Arkham.Timing qualified as Timing

newtype ToothOfEztli = ToothOfEztli AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

toothOfEztli :: AssetCard ToothOfEztli
toothOfEztli = asset ToothOfEztli Cards.toothOfEztli

instance HasModifiersFor ToothOfEztli where
  getModifiersFor (SkillTestSource _ _ (TreacherySource _) _) (InvestigatorTarget iid) (ToothOfEztli a)
    | controlledBy a iid
    = pure $ toModifiers
      a
      [SkillModifier SkillWillpower 1, SkillModifier SkillAgility 1]
  getModifiersFor _ _ _ = pure []

instance HasAbilities ToothOfEztli where
  getAbilities (ToothOfEztli x) =
    [ restrictedAbility
        x
        1
        OwnsThis
        (ReactionAbility
          (SkillTestResult
            Timing.After
            You
            (SkillTestOnTreachery AnyTreachery)
            (SuccessResult AnyValue)
          )
          (ExhaustCost $ toTarget x)
        )
    ]

instance RunMessage ToothOfEztli where
  runMessage msg a@(ToothOfEztli attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (DrawCards iid 1 False)
    _ -> ToothOfEztli <$> runMessage msg attrs
