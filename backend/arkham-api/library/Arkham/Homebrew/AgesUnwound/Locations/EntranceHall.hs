module Arkham.Homebrew.AgesUnwound.Locations.EntranceHall (entranceHall) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Query (getLead)
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.TheMyriadGentleman.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype EntranceHall = EntranceHall LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

entranceHall :: LocationCard EntranceHall
entranceHall = symbolLabel $ location EntranceHall Cards.entranceHall 3 (Static 0)

{- | "Forced - After you reveal Entrance Hall: At the end of the current round,
spawn 1[per_investigator] copies of The Myriad Gentleman at Entrance Hall."
-}
instance HasAbilities EntranceHall where
  getAbilities (EntranceHall a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ RevealLocation #after You (be a)

instance RunMessage EntranceHall where
  runMessage msg l@(EntranceHall attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      n <- perPlayer 1
      -- The card does not say whose deck the copies come from, so the guide's
      -- tiebreak applies: the lead investigator does it.
      lead <- getLead
      atEndOfRound attrs $ spawnMyriadCopiesAt lead n attrs.id
      pure l
    _ -> EntranceHall <$> liftRunMessage msg attrs
