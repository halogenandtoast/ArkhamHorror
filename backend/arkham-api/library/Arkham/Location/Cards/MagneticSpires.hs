module Arkham.Location.Cards.MagneticSpires (magneticSpires) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifyEach)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.ObsidianCanyons.Helpers
import Arkham.Trait (Trait (Firearm))

newtype MagneticSpires = MagneticSpires LocationAttrs
  deriving anyclass (IsLocation)
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
    extendRevealed1 a
      $ scenarioI18n
      $ withI18nTooltip "magneticSpires.swap"
      $ groupLimit PerGame
      $ restricted a 1 Here
      $ actionAbilityWithCost (GroupClueCost (PerPlayer 1) (be a))

instance RunMessage MagneticSpires where
  runMessage msg l@(MagneticSpires attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      -- "Choose an open sky up to 2 spaces away and swap Magnetic Spires with
      -- it." The clue cost is handled by the ability cost above.
      -- TODO: swapping with an "open sky" requires the Summit-deck / open-sky
      -- placeholder infrastructure (drawing open-sky cards, sliding/swapping grid
      -- locations), which has no engine support yet. Implement the swap once that
      -- infra exists.
      pure l
    _ -> MagneticSpires <$> liftRunMessage msg attrs
