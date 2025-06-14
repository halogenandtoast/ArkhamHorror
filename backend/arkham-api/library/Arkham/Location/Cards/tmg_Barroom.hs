module Arkham.Location.Cards.TMGBarroom (tmgBarroom) where

import Arkham.Prelude
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import
import Arkham.Matcher

newtype TMGBarroom = TMGBarroom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | 'Barroom' from The Midwinter Gala (#71011).
tmgBarroom :: LocationCard TMGBarroom
tmgBarroom =
  location
    TMGBarroom
    Cards.tmgBarroom
    3
    (PerPlayer 2)
    NoSymbol
    []
    & revealedBy False

instance HasAbilities TMGBarroom where
  getAbilities (TMGBarroom attrs) =
    withBaseAbilities attrs
      [ limitedAbility (GroupLimit PerRound 1)
          $ restrictedAbility attrs 1 Here
          $ FastAbility (ResourceCost 1)
      ]

instance RunMessage TMGBarroom where
  runMessage msg l@(TMGBarroom attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      -- TODO: Implement heal ability
      pure l
    _ -> TMGBarroom <$> runMessage msg attrs
