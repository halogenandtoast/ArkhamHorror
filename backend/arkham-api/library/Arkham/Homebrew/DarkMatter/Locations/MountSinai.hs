module Arkham.Homebrew.DarkMatter.Locations.MountSinai (mountSinai) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (cancelPendingScan, wouldScanEventAt)
import Arkham.Location.Import.Lifted hiding (PerformAction)
import Arkham.Matcher

newtype MountSinai = MountSinai LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mountSinai :: LocationCard MountSinai
mountSinai = symbolLabel $ location MountSinai Cards.mountSinai 4 (PerPlayer 1)

{- | "Forced - When you would scan at Mount Sinai: Test [agility] (1). This test
gets +2 difficulty for each clue on this location. If you fail, cancel that
scan."

Gated on the scan's anchor rather than 'Here': a remote "scan as if you were
at that location" (Universal Archives) must still trigger this, and an
ordinary scan performed while merely standing here for somewhere else must
not.
-}
instance HasAbilities MountSinai where
  getAbilities (MountSinai a) =
    extendRevealed1 a
      $ mkAbility a 1
      $ forced
      $ CampaignEvent #when (Just You) (wouldScanEventAt a.id)

instance RunMessage MountSinai where
  runMessage msg l@(MountSinai attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed $ 1 + 2 * attrs.clues)
      pure l
    FailedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      cancelPendingScan
      pure l
    _ -> MountSinai <$> liftRunMessage msg attrs
