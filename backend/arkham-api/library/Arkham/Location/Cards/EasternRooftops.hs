module Arkham.Location.Cards.EasternRooftops (easternRooftops) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelf)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.TheDoomOfArkhamPartII.Helpers (rooftopsReachConnecting)

newtype EasternRooftops = EasternRooftops LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

easternRooftops :: LocationCard EasternRooftops
easternRooftops = location EasternRooftops Cards.easternRooftops 3 (Static 1)

{- | "Easttown, Rivertown, and Southside are connected to Eastern Rooftops, and vice
versa." The forward direction is already printed on the card def as connection
symbols (Heart, connecting to Moon/Circle/Square); only the reverse has to be
granted, since none of those three lists the rooftops' symbol.

Matched by title so it holds whichever printing of a neighbourhood is in play — in
this scenario Rivertown is its Ruined version and Southside is one of two.
-}
neighbors :: LocationMatcher
neighbors = mapOneOf LocationWithTitle ["Easttown", "Rivertown", "Southside"]

instance HasModifiersFor EasternRooftops where
  getModifiersFor (EasternRooftops a) = do
    -- Printed on both sides of the card.
    modifySelf a [CannotBeFlooded]
    modifySelect a neighbors [ConnectedToWhen neighbors (be a)]

    -- "While you are at Eastern Rooftops, you may fight and evade enemies at
    -- connecting locations as if you were at their location." Revealed side only.
    whenRevealed a $ rooftopsReachConnecting a

    whenUnrevealed a do
      -- "As an additional cost to enter Eastern Rooftops, you must spend 1
      -- [per_investigator] clues, as a group." The cost belongs on the investigators
      -- who might enter, not on the location: one held by someone already standing
      -- here would never gate the move in.
      modifySelect
        a
        Anyone
        [AdditionalCostToEnterMatching (be a) (GroupClueCost (PerPlayer 1) Anywhere)]

instance RunMessage EasternRooftops where
  runMessage msg (EasternRooftops attrs) = runQueueT $ EasternRooftops <$> liftRunMessage msg attrs
