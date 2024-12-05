module Arkham.Location.Cards.UnvisitedIsleStandingStones (
  unvisitedIsleStandingStones,
  UnvisitedIsleStandingStones (..),
  unvisitedIsleStandingStonesEffect,
)
where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.CampaignLogKey
import Arkham.Effect.Import
import Arkham.GameValue
import Arkham.Helpers.Log
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest (getSkillTest, withSkillTest)
import Arkham.Location.Brazier
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.UnionAndDisillusion.Helpers

newtype UnvisitedIsleStandingStones = UnvisitedIsleStandingStones LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unvisitedIsleStandingStones :: LocationCard UnvisitedIsleStandingStones
unvisitedIsleStandingStones = location UnvisitedIsleStandingStones Cards.unvisitedIsleStandingStones 3 (PerPlayer 2)

instance HasModifiersFor UnvisitedIsleStandingStones where
  getModifiersFor (UnvisitedIsleStandingStones a) = whenUnrevealed a $ maybeModifySelf a do
    sidedWithLodge <- lift $ getHasRecord TheInvestigatorsSidedWithTheLodge
    isLit <- lift $ selectAny $ locationIs Locations.forbiddingShore <> LocationWithBrazier Lit
    guard $ if sidedWithLodge then not isLit else isLit
    pure [Blocked]

instance HasAbilities UnvisitedIsleStandingStones where
  getAbilities (UnvisitedIsleStandingStones attrs) =
    extendRevealed
      attrs
      [ restricted attrs 1 Here $ ActionAbility [Action.Circle] $ ActionCost 1
      , haunted
          "Until the end of the round, increase the difficulty of each skill test during a _circle_ action by 2."
          attrs
          2
      ]

instance RunMessage UnvisitedIsleStandingStones where
  runMessage msg l@(UnvisitedIsleStandingStones attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      circleTest sid iid (attrs.ability 1) attrs [#willpower, #intellect] (Fixed 10)
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      withSkillTest \sid ->
        createCardEffect Cards.unvisitedIsleStandingStones Nothing attrs (SkillTestTarget sid)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      passedCircleTest iid attrs
      pure l
    _ -> UnvisitedIsleStandingStones <$> liftRunMessage msg attrs

newtype UnvisitedIsleStandingStonesEffect = UnvisitedIsleStandingStonesEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unvisitedIsleStandingStonesEffect :: EffectArgs -> UnvisitedIsleStandingStonesEffect
unvisitedIsleStandingStonesEffect = cardEffect UnvisitedIsleStandingStonesEffect Cards.unvisitedIsleStandingStones

instance HasModifiersFor UnvisitedIsleStandingStonesEffect where
  getModifiersFor (UnvisitedIsleStandingStonesEffect a) = do
    getSkillTest >>= \case
      Just st ->
        if Action.Circle `elem` st.action
          then modified_ a (SkillTestTarget st.id) [Difficulty 2]
          else pure mempty
      Nothing -> pure mempty

instance RunMessage UnvisitedIsleStandingStonesEffect where
  runMessage msg e@(UnvisitedIsleStandingStonesEffect attrs) = runQueueT $ case msg of
    EndRound -> disableReturn e
    _ -> UnvisitedIsleStandingStonesEffect <$> liftRunMessage msg attrs
