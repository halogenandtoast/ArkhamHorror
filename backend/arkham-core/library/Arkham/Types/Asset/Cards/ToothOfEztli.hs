module Arkham.Types.Asset.Cards.ToothOfEztli
  ( toothOfEztli
  , ToothOfEztli(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Matcher
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Timing qualified as Timing

newtype ToothOfEztli = ToothOfEztli AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

toothOfEztli :: AssetCard ToothOfEztli
toothOfEztli = asset ToothOfEztli Cards.toothOfEztli

instance HasModifiersFor env ToothOfEztli where
  getModifiersFor (SkillTestSource _ _ (TreacherySource _) _ _) (InvestigatorTarget iid) (ToothOfEztli a)
    | ownedBy a iid
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

instance AssetRunner env => RunMessage env ToothOfEztli where
  runMessage msg a@(ToothOfEztli attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (DrawCards iid 1 False)
    _ -> ToothOfEztli <$> runMessage msg attrs
