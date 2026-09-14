module Arkham.Homebrew.DarkMatter.Locations.Earth (earth) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Log (getHasRecord, hasRecord)
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.DarkMatter.Key
import Arkham.Investigator.Types (Field (InvestigatorLog))
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Projection

newtype Earth = Earth LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

earth :: LocationCard Earth
earth = symbolLabel $ location Earth Cards.earth 3 (PerPlayer 2)

-- | "Forced - After you enter this location: You are immediately driven insane."
instance HasAbilities Earth where
  getAbilities (Earth a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ Enters #after You (be a)

instance RunMessage Earth where
  runMessage msg l@(Earth attrs) = runQueueT $ case msg of
    {- "Revelation - Put this location into play. Check your Campaign Log. Each
    investigator who has been corrupted by the Earth takes 2 damage."

    The engine puts the location into play when the scanned card is drawn, so the
    revelation only has the damage left to do. Corruption is recorded per
    investigator by In the Shadow of Earth's agenda 2b, and for the whole group by
    its resolution 5; either entry counts. -}
    Revelation _ (isSource attrs -> True) -> do
      allCorrupted <- getHasRecord AllInvestigatorsHaveBeenCorruptedByTheEarth
      eachInvestigator \iid -> do
        corrupted <- fieldMap InvestigatorLog (hasRecord HasBeenCorruptedByTheEarth) iid
        when (allCorrupted || corrupted) $ assignDamage iid attrs 2
      Earth <$> liftRunMessage msg attrs
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ DrivenInsane iid
      pure l
    _ -> Earth <$> liftRunMessage msg attrs
