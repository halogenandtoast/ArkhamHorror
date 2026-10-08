module Arkham.Location.Cards.TheInnsmouthConspiracy.DevilReef.SaltMarshes (saltMarshes, SaltMarshes (..)) where

import Arkham.Ability
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Helpers.Scenario
import Arkham.Key
import Arkham.Location.CardDefs.TheInnsmouthConspiracy.DevilReef qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Matcher qualified as Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Deck
import Arkham.Scenarios.TheInnsmouthConspiracy.DevilReef.Helpers (islandCells, otherSide, out)

newtype SaltMarshes = SaltMarshes LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

saltMarshes :: LocationCard SaltMarshes
saltMarshes = locationWith SaltMarshes Cards.saltMarshes 4 (Static 0) connectsToAdjacent

instance HasAbilities SaltMarshes where
  getAbilities (SaltMarshes a) =
    extendRevealed
      a
      [ groupLimit PerGame
          $ restricted
            a
            1
            ( Here
                <> youExist (InvestigatorWithKey PurpleKey)
                <> exists
                  (UnrevealedLocation <> mapOneOf LocationWithUnrevealedTitle ["Tidal Tunnel", "Unfathomable Depths"])
            )
            actionAbility
      , mkAbility a 2 $ forced $ Matcher.RevealLocation #after Anyone (be a)
      ]

instance RunMessage SaltMarshes where
  runMessage msg l@(SaltMarshes attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <-
        select
          $ UnrevealedLocation
          <> mapOneOf LocationWithUnrevealedTitle ["Tidal Tunnel", "Unfathomable Depths"]
      chooseTargetM iid locations $ lookAtRevealed iid (attrs.ability 1)
      pure l
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      increaseThisFloodLevel attrs
      tunnels <- take 1 <$> getScenarioDeck TidalTunnelDeck
      -- One tunnel, off to the island's side.
      positions <- islandCells attrs.id ([otherSide], [out])
      zipWithM_ placeLocationInGrid positions tunnels
      pure l
    _ -> SaltMarshes <$> liftRunMessage msg attrs
