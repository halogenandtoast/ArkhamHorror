module Arkham.Location.Cards.DazzlingSkyline (dazzlingSkyline) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.ObsidianCanyons.Helpers
import Arkham.Window (getBatchId)

newtype DazzlingSkyline = DazzlingSkyline LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dazzlingSkyline :: LocationCard DazzlingSkyline
dazzlingSkyline = location DazzlingSkyline Cards.dazzlingSkyline 1 (Static 1)

instance HasAbilities DazzlingSkyline where
  getAbilities (DazzlingSkyline a) =
    if a.revealed
      then
        extendRevealed
          a
          [ mkAbility a 1 $ forced $ RevealLocation #after You (be a)
          , restricted a 2 Here $ actionAbilityWithCost $ AtLeastOne (Fixed 3) (ClueCost $ Static 1)
          ]
      else extendUnrevealed1 a (summitEntry a 9)

instance RunMessage DazzlingSkyline where
  runMessage msg l@(DazzlingSkyline attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 9 (getBatchId -> batchId) _ -> do
      summitEntryToll attrs 9 iid batchId
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 9 -> True) -> do
      summitEntryFailed attrs 9 iid
      pure l
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid (attrs.ability 1) 1
      pure l
    UseCardAbility _iid (isSource attrs -> True) 2 _ (totalCluePayment -> n) -> do
      doStep n msg
      pure l
    DoStep n (UseThisAbility iid (isSource attrs -> True) 2) | n > 0 -> do
      revealed <- drawFromSummitBottom 3
      placeOnSummitTopOrBottom iid revealed
      doNextStep msg
      pure l
    _ -> DazzlingSkyline <$> liftRunMessage msg attrs
