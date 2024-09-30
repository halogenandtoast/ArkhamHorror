module Arkham.Location.Cards.SaltMarshes (saltMarshes, SaltMarshes (..)) where

import Arkham.Ability
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Helpers.Scenario
import Arkham.Key
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Grid
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Matcher qualified as Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Deck

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
      grid <- getGrid
      tunnels <- take 1 <$> getScenarioDeck TidalTunnelDeck

      let
        p1 = case findInGrid attrs.id grid of
          Just (Pos 0 3) -> Pos (-1) 3
          Just (Pos 4 2) -> Pos 5 2
          Just (Pos 4 (-2)) -> Pos 5 (-2)
          Just (Pos (-4) 2) -> Pos (-5) 2
          Just (Pos (-4) (-2)) -> Pos (-5) (-2)
          _ -> error "invalid location"
      zipWithM_ placeLocationInGrid [p1] tunnels
      pure l
    _ -> SaltMarshes <$> liftRunMessage msg attrs
