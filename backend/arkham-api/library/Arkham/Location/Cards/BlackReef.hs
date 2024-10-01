module Arkham.Location.Cards.BlackReef (blackReef, BlackReef (..)) where

import Arkham.Ability
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Card.CardType
import Arkham.Helpers.Scenario
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Grid
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Matcher qualified as Matcher
import Arkham.Scenario.Deck

newtype BlackReef = BlackReef LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blackReef :: LocationCard BlackReef
blackReef = locationWith BlackReef Cards.blackReef 2 (Static 0) connectsToAdjacent

instance HasAbilities BlackReef where
  getAbilities (BlackReef a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ Matcher.RevealLocation #after Anyone (be a)

instance RunMessage BlackReef where
  runMessage msg l@(BlackReef attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      increaseThisFloodLevel attrs
      grid <- getGrid
      tunnels <- take 2 <$> getScenarioDeck TidalTunnelDeck

      let
        (p1, p2, p3) = case findInGrid attrs.id grid of
          Just (Pos 0 3) -> (Pos 1 3, Pos 0 4, Pos 1 4)
          Just (Pos 4 2) -> (Pos 5 2, Pos 4 1, Pos 5 1)
          Just (Pos 4 (-2)) -> (Pos 5 (-2), Pos 4 (-1), Pos 5 (-1))
          Just (Pos (-4) 2) -> (Pos (-5) 2, Pos (-4) 1, Pos (-5) 1)
          Just (Pos (-4) (-2)) -> (Pos (-5) (-2), Pos (-4) (-1), Pos (-5) (-1))
          _ -> error "invalid location"
      zipWithM_ placeLocationInGrid [p1, p2] tunnels
      unfathomableDepths <- getSetAsideCardsMatching $ CardWithType LocationType
      shuffleM unfathomableDepths >>= \case
        [] -> pure ()
        (x : _) -> placeLocationInGrid_ p3 x
      pure l
    _ -> BlackReef <$> liftRunMessage msg attrs
