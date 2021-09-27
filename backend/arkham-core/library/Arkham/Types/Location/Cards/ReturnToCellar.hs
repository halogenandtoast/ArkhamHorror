module Arkham.Types.Location.Cards.ReturnToCellar
  ( returnToCellar
  , ReturnToCellar(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (RevealLocation)
import Arkham.Types.Timing qualified as Timing

newtype ReturnToCellar = ReturnToCellar LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToCellar :: LocationCard ReturnToCellar
returnToCellar = location
  ReturnToCellar
  Cards.returnToCellar
  2
  (PerPlayer 1)
  Plus
  [Square, Squiggle]

instance HasAbilities ReturnToCellar where
  getAbilities (ReturnToCellar attrs) =
    withBaseAbilities attrs
      $ [ mkAbility attrs 1
          $ ForcedAbility
          $ RevealLocation Timing.After You
          $ LocationWithId
          $ toId attrs
        | locationRevealed attrs
        ]

instance (LocationRunner env) => RunMessage env ReturnToCellar where
  runMessage msg l@(ReturnToCellar attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      deepBelowYourHouse <- getSetAsideCard Cards.deepBelowYourHouse
      l <$ push (PlaceLocation deepBelowYourHouse)
    _ -> ReturnToCellar <$> runMessage msg attrs
