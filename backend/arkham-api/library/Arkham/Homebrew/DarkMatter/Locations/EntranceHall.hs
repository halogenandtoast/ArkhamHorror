module Arkham.Homebrew.DarkMatter.Locations.EntranceHall (entranceHall) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (switchedEventFor)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype EntranceHall = EntranceHall LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

entranceHall :: LocationCard EntranceHall
entranceHall = locationWith EntranceHall Cards.entranceHall 2 (PerPlayer 2) connectsToAdjacent

{- | Unrevealed (A Shimmer in the Wall) side: "You cannot enter A Shimmer in the
Wall except through the [fast] ability on Maja."

This has to be a modifier rather than criteria on the location's own move
ability, or Safeguard/Elusive/Pathfinder walk right in. 'CannotEnter' would be
too strong — it is sourceless, so it would also block Maja — hence
'CannotEnterExcept', which is checked against the source of the effect doing the
moving.
-}
instance HasModifiersFor EntranceHall where
  getModifiersFor (EntranceHall a) =
    whenUnrevealed a
      $ modifySelect a Anyone [CannotEnterExcept a.id (SourceIsAsset $ assetIs Assets.maja)]

{- | "[reaction] After Entrance Hall is switched with another location, if you are
at Entrance Hall: Draw 1 card and gain 1 resource. (Group limit once per round.)"

Only a switch that Entrance Hall itself was part of counts, so the ability
matches the location-keyed window rather than the broad @switched@ one — a
handler-side check would still offer the reaction (and burn the group limit)
after every switch elsewhere on the map.
-}
instance HasAbilities EntranceHall where
  getAbilities (EntranceHall a) =
    extendRevealed1 a
      $ groupLimit PerRound
      $ restricted a 1 Here
      $ freeReaction
      $ ScenarioEvent #after Nothing (switchedEventFor a.id)

instance RunMessage EntranceHall where
  runMessage msg l@(EntranceHall attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawCards iid (attrs.ability 1) 1
      gainResources iid (attrs.ability 1) 1
      pure l
    _ -> EntranceHall <$> liftRunMessage msg attrs
