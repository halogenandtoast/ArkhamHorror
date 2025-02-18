module Arkham.Location.Helpers (module Arkham.Location.Helpers) where

import Arkham.Ability
import Arkham.Card.CardCode
import Arkham.Direction
import Arkham.Location.Base
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Source

resignAction :: (HasCardCode a, Sourceable a) => a -> Ability
resignAction a = restricted a 99 Here $ ActionAbility [#resign] (ActionCost 1)

drawCardUnderneathAction :: (HasCardCode a, Sourceable a) => a -> Ability
drawCardUnderneathAction a =
  groupLimit PerGame
    $ restricted a 100 (Here <> exists (YourLocation <> LocationWithoutClues))
    $ FastAbility Free

adjacentLocations :: Set Direction
adjacentLocations = setFromList [minBound .. maxBound]

connectsToAdjacent :: LocationAttrs -> LocationAttrs
connectsToAdjacent = connectsToL .~ adjacentLocations
