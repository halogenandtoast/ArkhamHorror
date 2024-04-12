module Arkham.Location.Cards.UnvisitedIsleStandingStones (
  unvisitedIsleStandingStones,
  UnvisitedIsleStandingStones (..),
  unvisitedIsleStandingStonesEffect,
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

newtype UnvisitedIsleStandingStones = UnvisitedIsleStandingStones LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unvisitedIsleStandingStones :: LocationCard UnvisitedIsleStandingStones
unvisitedIsleStandingStones = location UnvisitedIsleStandingStones Cards.unvisitedIsleStandingStones 3 (PerPlayer 2)

instance HasModifiersFor UnvisitedIsleStandingStones where
  getModifiersFor target (UnvisitedIsleStandingStones attrs)
    | attrs `isTarget` target
    , not (locationRevealed attrs) = do
        sidedWithLodge <- getHasRecord TheInvestigatorsSidedWithTheLodge
        isLit <- selectAny $ locationIs Locations.forbiddingShore <> LocationWithBrazier Lit
        pure
          [ toModifier attrs Blocked
          | if sidedWithLodge then not isLit else isLit
          ]
  getModifiersFor _ _ = pure []

instance HasAbilities UnvisitedIsleStandingStones where
  getAbilities (UnvisitedIsleStandingStones attrs) =
    withRevealedAbilities
      attrs
      [ restrictedAbility attrs 1 Here $ ActionAbility ([Action.Circle]) $ ActionCost 1
      , haunted
          "Until the end of the round, increase the difficulty of each skill test during a _circle_ action by 2."
          attrs
          2
      ]

instance RunMessage UnvisitedIsleStandingStones where
  runMessage msg l@(UnvisitedIsleStandingStones attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      circleTest iid attrs attrs [#willpower, #intellect] (Fixed 10)
      pure l
    UseCardAbility _ (isSource attrs -> True) 2 _ _ -> do
      push $ createCardEffect Cards.unvisitedIsleStandingStones Nothing attrs SkillTestTarget
      pure l
    PassedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ _ -> do
      passedCircleTest iid attrs
      pure l
    _ -> UnvisitedIsleStandingStones <$> runMessage msg attrs

newtype UnvisitedIsleStandingStonesEffect = UnvisitedIsleStandingStonesEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unvisitedIsleStandingStonesEffect :: EffectArgs -> UnvisitedIsleStandingStonesEffect
unvisitedIsleStandingStonesEffect = cardEffect UnvisitedIsleStandingStonesEffect Cards.unvisitedIsleStandingStones

instance HasModifiersFor UnvisitedIsleStandingStonesEffect where
  getModifiersFor SkillTestTarget (UnvisitedIsleStandingStonesEffect a) = do
    mAction <- getSkillTestAction
    case mAction of
      Just Action.Circle -> pure $ toModifiers a [Difficulty 2]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance RunMessage UnvisitedIsleStandingStonesEffect where
  runMessage msg e@(UnvisitedIsleStandingStonesEffect attrs@EffectAttrs {..}) = case msg of
    EndRound -> do
      push $ DisableEffect effectId
      pure e
    _ -> UnvisitedIsleStandingStonesEffect <$> runMessage msg attrs
