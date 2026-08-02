module Arkham.Location.Cards.DazzlingSkyline (dazzlingSkyline) where

import Arkham.Ability
import Arkham.Capability
import Arkham.Investigator.Types (Field (InvestigatorClues))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Projection
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
          , restricted a 2 (Here <> youExist can.spend.clues) actionAbility
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
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      -- Spend 1-3 clues, capped at what the investigator can actually pay.
      clues <- field InvestigatorClues iid
      chooseAmount iid "Clues" "Clues" 1 (min 3 clues) attrs
      pure l
    ResolveAmounts iid (getChoiceAmount "Clues" -> n) (isTarget attrs -> True) | n > 0 -> do
      spendClues iid n
      -- "For each clue spent, reveal the bottom 3 cards of the Summit deck."
      -- Resolved one batch at a time so the investigator can react to what the
      -- last batch turned up before committing the next.
      replicateM_ n $ forInvestigator iid (DoStep 1 msg)
      pure l
    ForInvestigator iid (DoStep 1 (ResolveAmounts _ _ (isTarget attrs -> True))) -> do
      revealed <- drawFromSummitBottom 3
      placeOnSummitTopOrBottom iid revealed
      pure l
    _ -> DazzlingSkyline <$> liftRunMessage msg attrs
