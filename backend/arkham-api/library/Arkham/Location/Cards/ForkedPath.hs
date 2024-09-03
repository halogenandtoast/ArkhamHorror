module Arkham.Location.Cards.ForkedPath (
  forkedPath,
  ForkedPath (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype ForkedPath = ForkedPath LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forkedPath :: LocationCard ForkedPath
forkedPath = symbolLabel $ location ForkedPath Cards.forkedPath 2 (PerPlayer 2)

-- Forced - When you explore while at Forked Path, check your supplies. If you
-- have a map: Continue drawing cards from the exploration deck until you have
-- drawn 2 connecting locations, if able. Put both of those locations into play
-- and choose 1 to move to

instance HasAbilities ForkedPath where
  getAbilities (ForkedPath attrs) =
    withBaseAbilities
      attrs
      [ restrictedAbility
          attrs
          1
          (Here <> InvestigatorExists (You <> InvestigatorWithSupply Map))
          $ ForcedAbility
          $ AttemptExplore Timing.When You
      ]

instance RunMessage ForkedPath where
  runMessage msg l@(ForkedPath attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      popMessageMatching_ $ \case
        Do (Explore {}) -> True
        _ -> False
      explore
        iid
        (toSource attrs)
        (CardWithPrintedLocationConnection $ locationSymbol attrs)
        PlaceExplored
        2
      pure l
    _ -> ForkedPath <$> runMessage msg attrs
