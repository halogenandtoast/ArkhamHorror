module Arkham.Location.Cards.StoneArchways (stoneArchways, StoneArchways (..)) where

import Arkham.Ability
import Arkham.Direction
import Arkham.Draw.Types
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenario.Deck
import Arkham.Scenarios.ThePallidMask.Helpers

newtype StoneArchways = StoneArchways LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stoneArchways :: LocationCard StoneArchways
stoneArchways =
  locationWith StoneArchways Cards.stoneArchways 2 (Static 0)
    $ (connectsToL .~ adjacentLocations)
    . (costToEnterUnrevealedL .~ GroupClueCost (PerPlayer 1) YourLocation)

instance HasModifiersFor StoneArchways where
  getModifiersFor (StoneArchways a) = do
    modifySelect
      a
      (UnrevealedLocation <> oneOf [LocationInDirection dir (be a) | dir <- [minBound ..]])
      [Blank]

instance HasAbilities StoneArchways where
  getAbilities (StoneArchways attrs) =
    extendRevealed1 attrs
      $ restricted attrs 1 (notExists $ rightOf attrs)
      $ forced
      $ RevealLocation #when Anyone (be attrs)

instance RunMessage StoneArchways where
  runMessage msg l@(StoneArchways attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ DrawCards iid $ targetCardDraw attrs CatacombsDeck 1
      pure l
    DrewCards _ drewCards | maybe False (isTarget attrs) drewCards.target -> do
      case drewCards.cards of
        [card] -> do
          msgs <- placeAtDirection RightOf attrs >>= \f -> f card
          pushAll msgs
        [] -> pure ()
        _ -> error "wrong number of cards drawn"
      pure l
    _ -> StoneArchways <$> liftRunMessage msg attrs
