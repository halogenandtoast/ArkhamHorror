module Arkham.Homebrew.DarkMatter.Locations.ColdWastes (coldWastes) where

import Arkham.Cost
import Arkham.GameValue
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.DarkMatter.Actions (pattern Scan)
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

newtype ColdWastes = ColdWastes LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

coldWastes :: LocationCard ColdWastes
coldWastes = symbolLabel $ location ColdWastes Cards.coldWastes 3 (Static 1)

{- | "As an additional cost to scan at Cold Wastes, you must either (choose one):
Spend 2 resources, or choose and discard 2 cards from your hand."

The cost rides on the location, so it applies to any Scan performed while here
regardless of which card raised the action.
-}
instance HasModifiersFor ColdWastes where
  getModifiersFor (ColdWastes a) =
    modifySelf
      a
      [ AdditionalCostToPerformAction (IsAction Scan)
          $ OrCost [ResourceCost 2, HandDiscardCost 2 #any]
      ]

instance RunMessage ColdWastes where
  runMessage msg (ColdWastes attrs) = ColdWastes <$> runMessage msg attrs
