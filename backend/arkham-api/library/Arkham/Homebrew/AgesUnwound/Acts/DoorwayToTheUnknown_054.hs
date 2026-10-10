module Arkham.Homebrew.AgesUnwound.Acts.DoorwayToTheUnknown_054 (doorwayToTheUnknown_054) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Asset.Types (Field (AssetResources))
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Homebrew.AgesUnwound.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.AgesUnwound.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgesUnwound.Helpers (recordTheTimeFor)
import Arkham.Homebrew.AgesUnwound.Key
import Arkham.Matcher
import Arkham.Message.Lifted.Log (record)

newtype DoorwayToTheUnknown_054 = DoorwayToTheUnknown_054 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

doorwayToTheUnknown_054 :: ActCard DoorwayToTheUnknown_054
doorwayToTheUnknown_054 = act (2, A) DoorwayToTheUnknown_054 Cards.doorwayToTheUnknown_054 Nothing

{- | "Objective - If there are 4 resources on Unstable Warding, you must
immediately advance."

The other way out of this act is /Backfire/ (@:ages-unwound:231@), the warding's
own reverse, which records "the investigators unleashed chaos" and advances act
deck 1 itself.
-}
instance HasAbilities DoorwayToTheUnknown_054 where
  getAbilities (DoorwayToTheUnknown_054 a) =
    [ restricted a 1 (exists wardedDoorIsSpent)
        $ Objective
        $ forced AnyWindow
    ]

{- | "if there are 4 resources on Unstable Warding" -- by def, because the
warding is unique to the @night_of_the_ritual@ set that both Scenario III and
Scenario VI gather.
-}
wardedDoorIsSpent :: AssetMatcher
wardedDoorIsSpent =
  assetIs Assets.unstableWarding <> AssetWithTokens (AtLeast $ Static 4) #resource

instance RunMessage DoorwayToTheUnknown_054 where
  runMessage msg a@(DoorwayToTheUnknown_054 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      wardSpent <- selectAny wardedDoorIsSpent

      {- "If there are 4 resources on Unstable Warding: ... record that the
      investigators dispelled the ward. Next to this, record the time. Remove
      Unstable Warding from the game." -}
      when wardSpent do
        record TheInvestigatorsDispelledTheWard
        recordTheTimeFor TheInvestigatorsDispelledTheWard
        selectEach (assetIs Assets.unstableWarding) removeFromGame

      {- "However you advanced: Put the set aside Ritual Circle (A Harnessed
      Future) into play. Spawn the set aside The Myriad Gentleman (The High
      Priest) at the Ritual Circle."

      The circle is placed before the clue arithmetic because the "unleashed
      chaos" branch places clues "as it enters play". -}
      ritualCircle <- placeSetAsideLocation Locations.ritualCircle_224

      {- "If the investigators unleashed chaos: Place 4[per_investigator] clues on
      Ritual Circle (A Harnessed Future) as it enters play. Then, remove
      1[per_investigator] clues from Ritual Circle (A Harnessed Future) for each
      resource on Unstable Warding."

      /Backfire/ records that key as it advances this act, so this is the
      warding-blew-up branch. The clues the card puts on are on top of the
      location's own printed 1[per_investigator]: "place ... as it enters play"
      is the engine's and the game's additive wording, and the alternative
      (4[per_investigator] replacing the printed value) would make the chaos
      branch *easier* than the clean one at three or more resources. Flagged as a
      reading, not a certainty. -}
      whenM (getHasRecord TheInvestigatorsUnleashedChaos) do
        extra <- perPlayer 4
        placeTokens attrs ritualCircle #clue extra

        perInvestigator <- perPlayer 1
        resources <- selectSum AssetResources (assetIs Assets.unstableWarding)
        removeTokens attrs ritualCircle #clue (perInvestigator * resources)

      createSetAsideEnemy_ Enemies.theMyriadGentleman_233 ritualCircle

      -- "Advance to Act 3a - Breaking the Circle."
      advanceToAct attrs Cards.breakingTheCircle A
      pure a
    _ -> DoorwayToTheUnknown_054 <$> liftRunMessage msg attrs
