module Arkham.Homebrew.AgesUnwound.Acts.DoorwayToTheUnknown_162 (doorwayToTheUnknown_162) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Asset.Types (Field (AssetResources))
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Homebrew.AgesUnwound.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.AgesUnwound.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgesUnwound.Key
import Arkham.Matcher

newtype DoorwayToTheUnknown_162 = DoorwayToTheUnknown_162 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | Act 3a on the rear-corridors branch.
doorwayToTheUnknown_162 :: ActCard DoorwayToTheUnknown_162
doorwayToTheUnknown_162 = act (3, A) DoorwayToTheUnknown_162 Cards.doorwayToTheUnknown_162 Nothing

{- | "Objective - If there are 4 resources on Unstable Warding, you must
immediately advance."

The other way out of this act is /Backfire/ (@:ages-unwound:231b@), the warding's
own reverse, which records "the investigators unleashed chaos" and advances act
deck 1 itself -- deliberately by deck id, because this scenario runs two act decks.
-}
instance HasAbilities DoorwayToTheUnknown_162 where
  getAbilities (DoorwayToTheUnknown_162 a) =
    [restricted a 1 (exists wardedDoorIsSpent) $ Objective $ forced AnyWindow]

{- | "if there are 4 resources on Unstable Warding" -- by def, because the warding
is unique to the @night_of_the_ritual@ set that both Scenario III and Scenario VI
gather.
-}
wardedDoorIsSpent :: AssetMatcher
wardedDoorIsSpent =
  assetIs Assets.unstableWarding <> AssetWithTokens (AtLeast $ Static 4) #resource

instance RunMessage DoorwayToTheUnknown_162 where
  runMessage msg a@(DoorwayToTheUnknown_162 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      wardSpent <- selectAny wardedDoorIsSpent

      {- "If there are 4 resources on Unstable Warding: Remove Unstable Warding
      from the game." -}
      when wardSpent $ selectEach (assetIs Assets.unstableWarding) removeFromGame

      {- "However you advanced: Put each set-aside Ritual Circle location into
      play. Spawn the set aside The Myriad Gentleman (The High Priest) at the
      Ritual Circle (A Harnessed Future)."

      The circles go down before the clue arithmetic because the "unleashed chaos"
      branch places clues "as it enters play". /A Harnessed Future/ may have been
      removed from the game by setup (/the investigators stepped into the
      future/), in which case there are no clues to place and the High Priest
      spawns at another [[Ritual]] circle -- his movement ban strands him anywhere
      else. -}
      placeSetAsideLocations_ [Locations.ritualCircle_173]
      for_ [Locations.ritualCircle_224, Locations.ritualCircle_225] \def ->
        whenM (selectAny $ SetAsideCardMatch $ cardIs def) $ placeSetAsideLocation_ def

      harnessedFuture <- selectOne $ locationIs Locations.ritualCircle_224

      {- "If /the investigators unleashed chaos:/ Place 4[per_investigator] clues
      on Ritual Circle (A Harnessed Future) as it enters play. Then, remove
      1[per_investigator] clues from Ritual Circle (A Harnessed Future) for each
      resource on Unstable Warding."

      The clues the card puts on are on top of the location's own printed
      1[per_investigator]: "place ... as it enters play" is the engine's and the
      game's additive wording. Flagged as a reading, not a certainty -- Scenario
      III's act 2b reads it the same way. -}
      whenM (getHasRecord TheInvestigatorsUnleashedChaos) do
        for_ harnessedFuture \circle -> do
          extra <- perPlayer 4
          placeTokens attrs circle #clue extra

          perInvestigator <- perPlayer 1
          resources <- selectSum AssetResources (assetIs Assets.unstableWarding)
          removeTokens attrs circle #clue (perInvestigator * resources)

      circles <- select $ LocationWithTitle "Ritual Circle"
      for_ (harnessedFuture <|> listToMaybe circles)
        $ createSetAsideEnemy_ Enemies.theMyriadGentleman_233

      -- "Advance to Act 4a - Breaking the Circles."
      advanceToAct attrs Cards.breakingTheCircles A
      pure a
    _ -> DoorwayToTheUnknown_162 <$> liftRunMessage msg attrs
