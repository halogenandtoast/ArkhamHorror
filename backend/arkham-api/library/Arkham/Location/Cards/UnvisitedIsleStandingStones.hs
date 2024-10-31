module Arkham.Location.Cards.UnvisitedIsleStandingStones (
  unvisitedIsleStandingStones,
  UnvisitedIsleStandingStones (..),
  unvisitedIsleStandingStonesEffect,
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

newtype UnvisitedIsleStandingStones = UnvisitedIsleStandingStones LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unvisitedIsleStandingStones :: LocationCard UnvisitedIsleStandingStones
unvisitedIsleStandingStones = location UnvisitedIsleStandingStones Cards.unvisitedIsleStandingStones 3 (PerPlayer 2)

instance HasModifiersFor UnvisitedIsleStandingStones where
  getModifiersFor target (UnvisitedIsleStandingStones attrs) = maybeModified attrs do
    guard $ attrs `isTarget` target
    guard $ not attrs.revealed
    sidedWithLodge <- lift $ getHasRecord TheInvestigatorsSidedWithTheLodge
    isLit <- lift $ selectAny $ locationIs Locations.forbiddingShore <> LocationWithBrazier Lit
    guard $ if sidedWithLodge then not isLit else isLit
    pure [Blocked]

instance HasAbilities UnvisitedIsleStandingStones where
  getAbilities (UnvisitedIsleStandingStones attrs) =
    extendRevealed
      attrs
      [ restrictedAbility attrs 1 Here $ ActionAbility ([Action.Circle]) $ ActionCost 1
      , haunted
          "Until the end of the round, increase the difficulty of each skill test during a _circle_ action by 2."
          attrs
          2
      ]

instance RunMessage UnvisitedIsleStandingStones where
  runMessage msg l@(UnvisitedIsleStandingStones attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      circleTest sid iid (attrs.ability 1) attrs [#willpower, #intellect] (Fixed 10)
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      withSkillTest \sid ->
        push =<< createCardEffect Cards.unvisitedIsleStandingStones Nothing attrs (SkillTestTarget sid)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      passedCircleTest iid attrs
      pure l
    _ -> UnvisitedIsleStandingStones <$> runMessage msg attrs

newtype UnvisitedIsleStandingStonesEffect = UnvisitedIsleStandingStonesEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unvisitedIsleStandingStonesEffect :: EffectArgs -> UnvisitedIsleStandingStonesEffect
unvisitedIsleStandingStonesEffect = cardEffect UnvisitedIsleStandingStonesEffect Cards.unvisitedIsleStandingStones

instance HasModifiersFor UnvisitedIsleStandingStonesEffect where
  getModifiersFor (SkillTestTarget _) (UnvisitedIsleStandingStonesEffect a) = do
    mAction <- getSkillTestAction
    case mAction of
      Just Action.Circle -> toModifiers a [Difficulty 2]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance RunMessage UnvisitedIsleStandingStonesEffect where
  runMessage msg e@(UnvisitedIsleStandingStonesEffect attrs@EffectAttrs {..}) = case msg of
    EndRound -> do
      push $ DisableEffect effectId
      pure e
    _ -> UnvisitedIsleStandingStonesEffect <$> runMessage msg attrs
