module Arkham.Location.Cards.UnvisitedIsleMossCoveredSteps (
  unvisitedIsleMossCoveredSteps,
  unvisitedIsleMossCoveredStepsEffect,
  UnvisitedIsleMossCoveredSteps (..),
)
where

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
import Arkham.Prelude
import Arkham.Scenarios.UnionAndDisillusion.Helpers

newtype UnvisitedIsleMossCoveredSteps = UnvisitedIsleMossCoveredSteps LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unvisitedIsleMossCoveredSteps :: LocationCard UnvisitedIsleMossCoveredSteps
unvisitedIsleMossCoveredSteps = location UnvisitedIsleMossCoveredSteps Cards.unvisitedIsleMossCoveredSteps 4 (PerPlayer 2)

instance HasModifiersFor UnvisitedIsleMossCoveredSteps where
  getModifiersFor target (UnvisitedIsleMossCoveredSteps attrs) = maybeModified attrs do
    guard $ attrs `isTarget` target
    guard $ not attrs.revealed
    sidedWithLodge <- lift $ getHasRecord TheInvestigatorsSidedWithTheLodge
    isLit <- lift $ selectAny $ locationIs Locations.forbiddingShore <> LocationWithBrazier Lit
    guard $ if sidedWithLodge then not isLit else isLit
    pure [Blocked]

instance HasAbilities UnvisitedIsleMossCoveredSteps where
  getAbilities (UnvisitedIsleMossCoveredSteps attrs) =
    extendRevealed
      attrs
      [ restrictedAbility attrs 1 Here $ ActionAbility ([Action.Circle]) $ ActionCost 1
      , haunted "Your next move action this round costs 1 additional action" attrs 2
      ]

instance RunMessage UnvisitedIsleMossCoveredSteps where
  runMessage msg l@(UnvisitedIsleMossCoveredSteps attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      circleTest sid iid (attrs.ability 1) attrs [#combat, #agility] (Fixed 10)
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push =<< createCardEffect Cards.unvisitedIsleMossCoveredSteps Nothing attrs iid
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      passedCircleTest iid attrs
      pure l
    _ -> UnvisitedIsleMossCoveredSteps <$> runMessage msg attrs

newtype UnvisitedIsleMossCoveredStepsEffect = UnvisitedIsleMossCoveredStepsEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unvisitedIsleMossCoveredStepsEffect :: EffectArgs -> UnvisitedIsleMossCoveredStepsEffect
unvisitedIsleMossCoveredStepsEffect = cardEffect UnvisitedIsleMossCoveredStepsEffect Cards.unvisitedIsleMossCoveredSteps

instance HasModifiersFor UnvisitedIsleMossCoveredStepsEffect where
  getModifiersFor target (UnvisitedIsleMossCoveredStepsEffect a) | effectTarget a == target = do
    toModifiers a [AdditionalActionCostOf (IsAction Action.Move) 1]
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
