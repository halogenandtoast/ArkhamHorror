module Arkham.Location.Cards.TMGBedroom (tmgBedroom) where

import Arkham.Prelude
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import
import Arkham.Matcher

newtype TMGBedroom = TMGBedroom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | 'Bedroom' from The Midwinter Gala (#71012).
tmgBedroom :: LocationCard TMGBedroom
tmgBedroom =
  location
    TMGBedroom
    Cards.tmgBedroom
    3
    (PerPlayer 2)
    NoSymbol
    []
    & revealedBy False

instance HasAbilities TMGBedroom where
  getAbilities (TMGBedroom attrs) =
    withBaseAbilities attrs
      [ restrictedAbility attrs 1 Here $ ActionAbility Nothing
      ]

instance RunMessage TMGBedroom where
  runMessage msg l@(TMGBedroom attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      -- TODO: Implement agility fight ability
      pure l
    _ -> TMGBedroom <$> runMessage msg attrs
