module Arkham.Location.Cards.LodgeCellarWeveBeenExpectingYou (
  lodgeCellarWeveBeenExpectingYou,
  LodgeCellarWeveBeenExpectingYou (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Scenarios.ForTheGreaterGood.Helpers
import Arkham.Timing qualified as Timing

newtype LodgeCellarWeveBeenExpectingYou = LodgeCellarWeveBeenExpectingYou LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

lodgeCellarWeveBeenExpectingYou :: LocationCard LodgeCellarWeveBeenExpectingYou
lodgeCellarWeveBeenExpectingYou = location LodgeCellarWeveBeenExpectingYou Cards.lodgeCellarWeveBeenExpectingYou 3 (PerPlayer 1)

instance HasAbilities LodgeCellarWeveBeenExpectingYou where
  getAbilities (LodgeCellarWeveBeenExpectingYou attrs) =
    withRevealedAbilities
      attrs
      [mkAbility attrs 1 $ ForcedAbility $ RevealLocation Timing.After You $ LocationWithId $ toId attrs]

instance RunMessage LodgeCellarWeveBeenExpectingYou where
  runMessage msg l@(LodgeCellarWeveBeenExpectingYou attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      mKey <- getRandomKey
      for_ mKey $ \key ->
        push $ PlaceKey (toTarget attrs) key
      pure l
    _ -> LodgeCellarWeveBeenExpectingYou <$> runMessage msg attrs
