module Arkham.Location.Cards.GreatLiftActive (greatLiftActive) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Direction
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Grid
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (Field (LocationPosition))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Projection
import Arkham.Scenarios.CourtOfTheAncients.Helpers

newtype GreatLiftActive = GreatLiftActive LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

greatLiftActive :: LocationCard GreatLiftActive
greatLiftActive = location GreatLiftActive Cards.greatLiftActive 2 (Static 0)

-- "Great Lift is connected to the locations to the right and left of it, and
-- vice versa." Shared with the (Inactive) face and driven by the lift's live
-- grid position, so the connections follow it as it slides.
--
-- This replaced a static connectsTo [LeftOf, RightOf], which the Swap in
-- ReplaceLocation overwrote with the (Inactive) side's empty connectsTo the
-- moment the lift flipped -- leaving the active lift with no connections at all.
instance HasModifiersFor GreatLiftActive where
  getModifiersFor (GreatLiftActive a) = greatLiftConnections a

instance HasAbilities GreatLiftActive where
  getAbilities (GreatLiftActive a) =
    extendRevealed1 a
      $ restricted a 1 Here
      $ freeReaction (SuccessfulInvestigation #when You (be a))

{- | Slide the Great Lift one level in the given direction, carrying all of its
cards/tokens/investigators (they stay attached to the same LocationId, so
@PlaceGrid@ preserves them). Mirrors @slideGreatLiftDown@ in WestAntechamber.
-}
slideGreatLift :: ReverseQueue m => LocationAttrs -> GridDirection -> m ()
slideGreatLift attrs dir = do
  pos <- fieldJust LocationPosition attrs.id
  push $ PlaceGrid (GridLocation (updatePosition pos dir) attrs.id)

{- | The directions the lift may currently slide. The tower is levels 1-4
(grid rows 0-3): it cannot slide above level 4 (row 3) or below level 1
(row 0).
-}
slideDirections :: ReverseQueue m => LocationAttrs -> m [GridDirection]
slideDirections attrs = do
  row <- fieldMap LocationPosition (maybe 0 positionRow) attrs.id
  pure $ [GridUp | row < 3] <> [GridDown | row > 0]

instance RunMessage GreatLiftActive where
  runMessage msg l@(GreatLiftActive attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      -- Replace the clue discovery: the engine redirects the successful
      -- investigation to our proxy target instead of discovering clues.
      withSkillTest \sid ->
        skillTestModifier
          sid
          (attrs.ability 1)
          (toTarget attrs)
          (AlternateSuccessfullInvestigation $ ProxyTarget (toTarget attrs) (toTarget attrs))
      pure l
    Successful (Action.Investigate, _) _ _ (ProxyTarget (isTarget attrs -> True) _) _ -> do
      doStep 1 msg
      pure l
    DoStep 1 s@(Successful (Action.Investigate, _) iid _ _ _) -> do
      -- Slide up or down once (free, mandatory). The chosen direction is carried
      -- into the additional slides via the step number.
      dirs <- slideDirections attrs
      chooseOneM iid $ scenarioI18n do
        for_ dirs \dir ->
          labeled' (slideLabel dir) $ slideGreatLift attrs dir >> doStep (additionalStep dir) s
      pure l
    DoStep n (Successful (Action.Investigate, _) iid _ _ _) | Just dir <- stepDirection n -> do
      -- "You may spend 1 [per_investigator] clues to slide up or down one
      -- additional time." This is a single additional slide in the direction
      -- already taken, offered only while the lift has somewhere left to go.
      dirs <- slideDirections attrs
      when (dir `elem` dirs) do
        chooseOneM iid $ scenarioI18n do
          labeled' (slideAdditionalLabel dir)
            $ withCost iid (GroupClueCost (PerPlayer 1) (be attrs))
            $ slideGreatLift attrs dir
          labeled' "greatLift.doNotSlide" nothing
      pure l
    _ -> GreatLiftActive <$> liftRunMessage msg attrs

{- | @DoStep@ carries only an @Int@, so the direction of the first (free) slide
is encoded in the step number to keep the single additional slide pointed the
same way.
-}
additionalStep :: GridDirection -> Int
additionalStep = \case
  GridUp -> 2
  _ -> 3

stepDirection :: Int -> Maybe GridDirection
stepDirection = \case
  2 -> Just GridUp
  3 -> Just GridDown
  _ -> Nothing

slideLabel :: GridDirection -> Text
slideLabel = \case
  GridUp -> "greatLift.slideUp"
  _ -> "greatLift.slideDown"

slideAdditionalLabel :: GridDirection -> Text
slideAdditionalLabel = \case
  GridUp -> "greatLift.slideUpAdditional"
  _ -> "greatLift.slideDownAdditional"
