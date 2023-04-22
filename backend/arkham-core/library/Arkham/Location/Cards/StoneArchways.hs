module Arkham.Location.Cards.StoneArchways
  ( stoneArchways
  , StoneArchways(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Direction
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Scenario.Deck
import Arkham.Scenarios.ThePallidMask.Helpers
import Arkham.Timing qualified as Timing

newtype StoneArchways = StoneArchways LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stoneArchways :: LocationCard StoneArchways
stoneArchways = locationWith
  StoneArchways
  Cards.stoneArchways
  2
  (Static 0)
  ((connectsToL .~ adjacentLocations)
  . (costToEnterUnrevealedL
    .~ Costs [ActionCost 1, GroupClueCost (PerPlayer 1) YourLocation]
    )
  )

instance HasModifiersFor StoneArchways where
  getModifiersFor (LocationTarget lid) (StoneArchways attrs) = do
    isUnrevealedAdjacent <- member lid <$> select
      (UnrevealedLocation <> LocationMatchAny
        [ LocationInDirection dir (LocationWithId $ toId attrs)
        | dir <- [minBound .. maxBound]
        ]
      )
    pure $ toModifiers attrs [ Blank | isUnrevealedAdjacent ]
  getModifiersFor _ _ = pure []

instance HasAbilities StoneArchways where
  getAbilities (StoneArchways attrs) = withBaseAbilities
    attrs
    [ restrictedAbility
        attrs
        1
        (Negate
          (LocationExists
          $ LocationInDirection RightOf (LocationWithId $ toId attrs)
          )
        )
      $ ForcedAbility
      $ RevealLocation Timing.When Anyone
      $ LocationWithId
      $ toId attrs
    | locationRevealed attrs
    ]

instance RunMessage StoneArchways where
  runMessage msg l@(StoneArchways attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push (DrawFromScenarioDeck iid CatacombsDeck (toTarget attrs) 1)
      pure l
    DrewFromScenarioDeck _ _ (isTarget attrs -> True) cards -> do
      case cards of
        [card] -> do
          msgs <- placeAtDirection RightOf attrs >>= \f -> f card
          pushAll msgs
        [] -> pure ()
        _ -> error "wrong number of cards drawn"
      pure l
    _ -> StoneArchways <$> runMessage msg attrs
