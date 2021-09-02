module Arkham.Types.Location.Cards.ReturnToAttic
  ( returnToAttic
  , ReturnToAttic(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (RevealLocation)
import qualified Arkham.Types.Timing as Timing

newtype ReturnToAttic = ReturnToAttic LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToAttic :: LocationCard ReturnToAttic
returnToAttic = location
  ReturnToAttic
  Cards.returnToAttic
  3
  (PerPlayer 1)
  Triangle
  [Square, Moon]

instance HasAbilities ReturnToAttic where
  getAbilities (ReturnToAttic attrs) =
    withBaseAbilities attrs $
      [ mkAbility attrs 1
        $ ForcedAbility
        $ RevealLocation Timing.After You
        $ LocationWithId
        $ toId attrs
      | locationRevealed attrs
      ]

instance (LocationRunner env) => RunMessage env ReturnToAttic where
  runMessage msg l@(ReturnToAttic attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      farAboveYourHouseId <- getRandom
      l <$ push (PlaceLocation farAboveYourHouseId Cards.farAboveYourHouse)
    _ -> ReturnToAttic <$> runMessage msg attrs
