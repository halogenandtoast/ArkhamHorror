module Arkham.Asset.Cards.TheCustodian
  ( theCustodian
  , TheCustodian(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Phase
import Arkham.Projection
import Arkham.SkillType
import Arkham.Target
import Arkham.Timing qualified as Timing

newtype TheCustodian = TheCustodian AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCustodian :: AssetCard TheCustodian
theCustodian = asset TheCustodian Cards.theCustodian

instance HasAbilities TheCustodian where
  getAbilities (TheCustodian a) =
    [ restrictedAbility a 1 ControlsThis
      $ ReactionAbility
          (PhaseBegins Timing.When $ PhaseIs InvestigationPhase)
          Free
    , restrictedAbility
        a
        2
        (Uncontrolled <> OnSameLocation <> InvestigatorExists
          (You <> InvestigatorWithAnyClues)
        )
      $ ActionAbility (Just Action.Parley)
      $ ActionCost 1
    ]

instance RunMessage TheCustodian where
  runMessage msg a@(TheCustodian attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      iids <- selectList $ InvestigatorAt $ locationWithAsset $ toId attrs
      for_ iids $ \iid -> do
        drawing <- drawCards iid attrs 1
        push drawing
      pure a
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      push $ beginSkillTest
        iid
        (toAbilitySource attrs 2)
        (toTarget attrs)
        (Just Action.Parley)
        SkillIntellect
        3
      pure a
    PassedSkillTest iid _ (isAbilitySource attrs 2 -> True) SkillTestInitiatorTarget{} _ _
      -> do
        clueCount <- field AssetClues (toId a)
        takeControl <- (clueCount >=) <$> getPlayerCountValue (PerPlayer 1)
        pushAll
          $ [InvestigatorSpendClues iid 1, PlaceClues (toTarget attrs) 1]
          <> [ TakeControlOfAsset iid (toId a) | takeControl ]
        pure a
    _ -> TheCustodian <$> runMessage msg attrs
