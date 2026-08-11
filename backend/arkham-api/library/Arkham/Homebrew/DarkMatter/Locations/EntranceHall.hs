module Arkham.Homebrew.DarkMatter.Locations.EntranceHall (entranceHall) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (getSwitchedLocations)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype EntranceHall = EntranceHall LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

entranceHall :: LocationCard EntranceHall
entranceHall = location EntranceHall Cards.entranceHall 2 (PerPlayer 2)

{- | "[reaction] After Entrance Hall is switched with another location, if you are
at Entrance Hall: Draw 1 card and gain 1 resource. (Group limit once per round.)"
-}
instance HasAbilities EntranceHall where
  getAbilities (EntranceHall a) =
    extendRevealed1 a
      $ groupLimit PerRound
      $ restricted a 1 Here
      $ freeReaction
      $ ScenarioEvent #after Nothing "switched"

instance RunMessage EntranceHall where
  runMessage msg l@(EntranceHall attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getSwitchedLocations -> Just (x, y)) _
      | attrs.id `elem` [x, y] -> do
          drawCards iid (attrs.ability 1) 1
          gainResources iid (attrs.ability 1) 1
          pure l
    _ -> EntranceHall <$> liftRunMessage msg attrs
