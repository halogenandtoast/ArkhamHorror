module Arkham.Location.Cards.UpstairsHallway (upstairsHallway, UpstairsHallway (..)) where

import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype UpstairsHallway = UpstairsHallway LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

upstairsHallway :: LocationCard UpstairsHallway
upstairsHallway = location UpstairsHallway Cards.upstairsHallway 2 (Static 0)

instance HasModifiersFor UpstairsHallway where
  getModifiersFor target (UpstairsHallway attrs) | attrs `is` target = do
    pure $ toModifiers attrs [Blocked | not (locationRevealed attrs)]
  getModifiersFor _ _ = pure []

instance HasAbilities UpstairsHallway where
  getAbilities (UpstairsHallway attrs) =
    extendRevealed
      attrs
      [ restrictedAbility attrs 1 (Here <> notExists (LocationWithTitle "Attic"))
          $ FastAbility
          $ GroupClueCost (PerPlayer 1) (LocationWithId $ toId attrs)
      ]

instance RunMessage UpstairsHallway where
  runMessage msg l@(UpstairsHallway attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ PlaceLocationMatching "Attic"
      pure l
    _ -> UpstairsHallway <$> runMessage msg attrs
