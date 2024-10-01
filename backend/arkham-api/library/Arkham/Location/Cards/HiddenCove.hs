module Arkham.Location.Cards.HiddenCove (hiddenCove, HiddenCove (..)) where

import Arkham.Ability
import Arkham.Card.CardType
import Arkham.Helpers.Scenario
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Grid
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenario.Deck

newtype HiddenCove = HiddenCove LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hiddenCove :: LocationCard HiddenCove
hiddenCove = locationWith HiddenCove Cards.hiddenCove 3 (Static 0) connectsToAdjacent

instance HasAbilities HiddenCove where
  getAbilities (HiddenCove a) =
    extendRevealed a [mkAbility a 1 $ forced $ RevealLocation #after Anyone (be a)]

instance RunMessage HiddenCove where
  runMessage msg l@(HiddenCove attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      grid <- getGrid
      tunnels <- getScenarioDeck TidalTunnelDeck
      unfathomableDepths <- getSetAsideCardsMatching $ CardWithType LocationType

      let
        (p1, p2) = case findInGrid attrs.id grid of
          Just (Pos 0 3) -> (Pos 0 4, Pos 1 4)
          Just (Pos 4 2) -> (Pos 5 2, Pos 6 2)
          Just (Pos 4 (-2)) -> (Pos 5 (-2), Pos 6 (-2))
          Just (Pos (-4) 2) -> (Pos (-5) 2, Pos (-6) 2)
          Just (Pos (-4) (-2)) -> (Pos (-5) (-2), Pos (-6) (-2))
          _ -> error "invalid location"
      case tunnels of
        [] -> pure ()
        (x : _) -> placeLocationInGrid_ p1 x

      shuffleM unfathomableDepths >>= \case
        [] -> pure ()
        (x : _) -> placeLocationInGrid_ p2 x
      pure l
    _ -> HiddenCove <$> liftRunMessage msg attrs
