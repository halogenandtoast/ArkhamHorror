{- | Wood Shed -- investigated with {combat} or {agility}.

The replacement rides the 'Investigate' message, which is the only place "the
skill indicated by the investigation attempt" exists: 'mkInvestigateLocation'
reads the indicated skill and every investigation of this location -- the basic
action and any card that investigates here -- passes through it.

Neither skill modifier fits. 'UseSkillInsteadOf' forces one skill and offers no
choice; 'UseSkillInPlaceOf' does offer one, but it is read by
@getAvailableSkillsFor@ inside @BeginSkillTestWithPreMessages'@, BEFORE the skill
test is installed, so a test-scoped modifier is invisible to it and an
investigator-scoped one would leak the swap into every other {combat} test taken
here (a fight, say). The attempt's own skill is the honest hook.
-}
module Arkham.Homebrew.AndThenThereWereNone.Locations.WoodShed (woodShed) where

import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Locations qualified as Cards
import Arkham.Investigate (withSkillType)
import Arkham.Location.Import.Lifted
import Arkham.Message.Lifted.Choose (chooseSkillM)

newtype WoodShed = WoodShed LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

woodShed :: LocationCard WoodShed
woodShed = location WoodShed Cards.woodShed 3 (PerPlayer 3)

instance RunMessage WoodShed where
  runMessage msg l@(WoodShed attrs) = runQueueT $ case msg of
    {- The action-paying copy is re-pushed by Investigator.Runner with
    @isAction@ cleared, so only the second one is intercepted -- and an attempt
    already indicating one of the two skills is left alone, both because it is
    already using one and because re-pushing it would re-enter this case. -}
    Investigate investigation
      | investigation.location == attrs.id
      , not investigation.isAction
      , investigation.skillType `notElem` [#combat, #agility] -> do
          chooseSkillM investigation.investigator [#combat, #agility] \sType ->
            push $ withSkillType sType investigation
          pure l
    _ -> WoodShed <$> liftRunMessage msg attrs
