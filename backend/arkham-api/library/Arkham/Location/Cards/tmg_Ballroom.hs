module Arkham.Location.Cards.TMGBallroom (tmgBallroom) where

import Arkham.Prelude
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import
import Arkham.Matcher

newtype TMGBallroom = TMGBallroom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | 'Ballroom' from The Midwinter Gala (#71010).
tmgBallroom :: LocationCard TMGBallroom
tmgBallroom =
  location
    TMGBallroom
    Cards.tmgBallroom
    2
    (PerPlayer 1)
    Square
    [Diamond, Spade, Triangle, Moon]
    & revealedBy False

instance HasAbilities TMGBallroom where
  getAbilities (TMGBallroom attrs) =
    withBaseAbilities attrs
      [ restrictedAbility attrs 1 Here $ ActionAbility (Just #parley)
      ]

instance RunMessage TMGBallroom where
  runMessage msg l@(TMGBallroom attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      -- TODO: Implement parley ability to exhaust guests and heal horror
      pure l
    _ -> TMGBallroom <$> runMessage msg attrs
