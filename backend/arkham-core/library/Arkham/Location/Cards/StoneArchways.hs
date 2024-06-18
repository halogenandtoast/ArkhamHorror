module Arkham.Location.Cards.StoneArchways (stoneArchways, StoneArchways (..)) where

import Arkham.Ability
import Arkham.Classes
import Arkham.Direction
import Arkham.Draw.Types
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Scenario.Deck
import Arkham.Scenarios.ThePallidMask.Helpers

newtype StoneArchways = StoneArchways LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stoneArchways :: LocationCard StoneArchways
stoneArchways =
  locationWith StoneArchways Cards.stoneArchways 2 (Static 0)
    $ (connectsToL .~ adjacentLocations)
    . (costToEnterUnrevealedL .~ Costs [ActionCost 1, GroupClueCost (PerPlayer 1) YourLocation])

instance HasModifiersFor StoneArchways where
  getModifiersFor (LocationTarget lid) (StoneArchways attrs) = do
    isUnrevealedAdjacent <-
      lid <=~> (UnrevealedLocation <> oneOf [LocationInDirection dir (be attrs) | dir <- [minBound ..]])
    pure $ toModifiers attrs [Blank | isUnrevealedAdjacent]
  getModifiersFor _ _ = pure []

instance HasAbilities StoneArchways where
  getAbilities (StoneArchways attrs) =
    extendRevealed
      attrs
      [ restrictedAbility attrs 1 (notExists $ LocationInDirection RightOf (be attrs))
          $ forced
          $ RevealLocation #when Anyone (be attrs)
      ]

instance RunMessage StoneArchways where
  runMessage msg l@(StoneArchways attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
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
    _ -> StoneArchways <$> runMessage msg attrs
