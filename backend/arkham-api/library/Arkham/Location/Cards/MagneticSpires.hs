module Arkham.Location.Cards.MagneticSpires (magneticSpires) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifyEach)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.ObsidianCanyons.Helpers
import Arkham.Trait (Trait (Firearm))
import Arkham.Window (getBatchId)

newtype MagneticSpires = MagneticSpires LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

magneticSpires :: LocationCard MagneticSpires
magneticSpires = location MagneticSpires Cards.magneticSpires 2 (Static 3)

instance HasModifiersFor MagneticSpires where
  getModifiersFor (MagneticSpires a) = do
    -- "While you are at Magnetic Spires, you cannot trigger abilities on Firearm
    -- assets you control." Modeled as a per-investigator-here restriction on
    -- abilities of Firearm assets.
    investigators <- select $ investigatorAt a
    modifyEach
      a
      investigators
      [CannotTriggerAbilityMatching (AbilityOnAsset (AssetWithTrait Firearm))]

instance HasAbilities MagneticSpires where
  getAbilities (MagneticSpires a) =
    if a.revealed
      then
        extendRevealed1 a
          $ scenarioI18n
          $ withI18nTooltip "magneticSpires.swap"
          $ groupLimit PerGame
          $ restricted a 1 Here
          $ actionAbilityWithCost (GroupClueCost (PerPlayer 1) (be a))
      else extendUnrevealed1 a (summitEntry a 9)

instance RunMessage MagneticSpires where
  runMessage msg l@(MagneticSpires attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 9 (getBatchId -> batchId) _ -> do
      summitEntryToll attrs 9 iid batchId
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 9 -> True) -> do
      summitEntryFailed attrs 9 iid
      pure l
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- "Choose an open sky up to 2 spaces away. Swap Magnetic Spires with the
      -- chosen open sky." The clue cost is paid by the ability cost above.
      candidates <- filterM (<=~> isOpenSky) =<< gridLocationsWithin 2 attrs.id
      chooseTargetM iid candidates $ swapGridPositions attrs.id
      pure l
    _ -> MagneticSpires <$> liftRunMessage msg attrs
