module Arkham.Location.Cards.ReturnToCellar (returnToCellar) where

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Timing qualified as Timing

newtype ReturnToCellar = ReturnToCellar LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToCellar :: LocationCard ReturnToCellar
returnToCellar = location ReturnToCellar Cards.returnToCellar 2 (PerPlayer 1)

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

instance RunMessage ReturnToCellar where
  runMessage msg l@(ReturnToCellar attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      placeDeepBelowYourHouse <- placeSetAsideLocation_ Cards.deepBelowYourHouse
      push placeDeepBelowYourHouse
      pure l
    _ -> ReturnToCellar <$> runMessage msg attrs
