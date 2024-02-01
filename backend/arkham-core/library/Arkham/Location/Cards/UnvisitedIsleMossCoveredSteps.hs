module Arkham.Location.Cards.UnvisitedIsleMossCoveredSteps (
  unvisitedIsleMossCoveredSteps,
  unvisitedIsleMossCoveredStepsEffect,
  UnvisitedIsleMossCoveredSteps (..),
)
where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.CampaignLogKey
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.GameValue
import Arkham.Helpers.Log
import Arkham.Helpers.Modifiers
import Arkham.Location.Brazier
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Scenarios.UnionAndDisillusion.Helpers

newtype UnvisitedIsleMossCoveredSteps = UnvisitedIsleMossCoveredSteps LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

unvisitedIsleMossCoveredSteps :: LocationCard UnvisitedIsleMossCoveredSteps
unvisitedIsleMossCoveredSteps = location UnvisitedIsleMossCoveredSteps Cards.unvisitedIsleMossCoveredSteps 4 (PerPlayer 2)

instance HasModifiersFor UnvisitedIsleMossCoveredSteps where
  getModifiersFor target (UnvisitedIsleMossCoveredSteps attrs)
    | attrs `isTarget` target
    , not (locationRevealed attrs) = do
        sidedWithLodge <- getHasRecord TheInvestigatorsSidedWithTheLodge
        isLit <- selectAny $ locationIs Locations.forbiddingShore <> LocationWithBrazier Lit
        pure
          [ toModifier attrs Blocked
          | if sidedWithLodge then not isLit else isLit
          ]
  getModifiersFor _ _ = pure []

instance HasAbilities UnvisitedIsleMossCoveredSteps where
  getAbilities (UnvisitedIsleMossCoveredSteps attrs) =
    withRevealedAbilities
      attrs
      [ restrictedAbility attrs 1 Here $ ActionAbility ([Action.Circle]) $ ActionCost 1
      , haunted "Your next move action this round costs 1 additional action" attrs 2
      ]

instance RunMessage UnvisitedIsleMossCoveredSteps where
  runMessage msg l@(UnvisitedIsleMossCoveredSteps attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      circleTest iid attrs attrs [#combat, #agility] 10
      pure l
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      push $ createCardEffect Cards.unvisitedIsleMossCoveredSteps Nothing attrs iid
      pure l
    PassedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ _ -> do
      passedCircleTest iid attrs
      pure l
    _ -> UnvisitedIsleMossCoveredSteps <$> runMessage msg attrs

newtype UnvisitedIsleMossCoveredStepsEffect = UnvisitedIsleMossCoveredStepsEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

unvisitedIsleMossCoveredStepsEffect :: EffectArgs -> UnvisitedIsleMossCoveredStepsEffect
unvisitedIsleMossCoveredStepsEffect = cardEffect UnvisitedIsleMossCoveredStepsEffect Cards.unvisitedIsleMossCoveredSteps

instance HasModifiersFor UnvisitedIsleMossCoveredStepsEffect where
  getModifiersFor target (UnvisitedIsleMossCoveredStepsEffect a) | effectTarget a == target = do
    pure $ toModifiers a [ActionCostOf (IsAction Action.Move) 1]
  getModifiersFor _ _ = pure []

instance RunMessage UnvisitedIsleMossCoveredStepsEffect where
  runMessage msg e@(UnvisitedIsleMossCoveredStepsEffect attrs) = case msg of
    TakenActions iid (elem #move -> True) | toTarget iid == effectTarget attrs -> do
      push $ DisableEffect $ toId attrs
      pure e
    EndRound -> do
      push $ DisableEffect $ toId attrs
      pure e
    _ -> UnvisitedIsleMossCoveredStepsEffect <$> runMessage msg attrs
