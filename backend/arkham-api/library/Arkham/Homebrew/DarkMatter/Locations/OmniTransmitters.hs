module Arkham.Homebrew.DarkMatter.Locations.OmniTransmitters (omniTransmitters) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Modifiers (ModifierType (Semaphore), semaphore)
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (addMemories)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype OmniTransmitters = OmniTransmitters LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

omniTransmitters :: LocationCard OmniTransmitters
omniTransmitters = symbolLabel $ location OmniTransmitters Cards.omniTransmitters 2 (PerPlayer 1)

{- | "[action] Add 1 doom to the current agenda and test [intellect] (2): If you
succeed, each investigator at this location adds 1 tally mark next to their
'Memories'. (Max one success per game.)"
-}
instance HasAbilities OmniTransmitters where
  getAbilities (OmniTransmitters a) =
    extendRevealed1 a $ skillTestAbility $ restricted a 1 Here actionAbility

instance RunMessage OmniTransmitters where
  runMessage msg l@(OmniTransmitters attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      placeDoomOnAgendaAndCheckAdvanceBy (attrs.ability 1) 1
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #intellect (Fixed 2)
      pure l
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      semaphore attrs do
        gameModifier (attrs.ability 1) attrs Semaphore
        selectEach (investigatorAt attrs.id) (`addMemories` 1)
      pure l
    _ -> OmniTransmitters <$> liftRunMessage msg attrs
