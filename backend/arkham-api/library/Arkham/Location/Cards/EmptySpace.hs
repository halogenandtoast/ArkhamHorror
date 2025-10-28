module Arkham.Location.Cards.EmptySpace (emptySpace) where

import Arkham.Ability
import Arkham.Capability
import Arkham.ForMovement
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.BeforeTheBlackThrone.Helpers

newtype EmptySpace = EmptySpace LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

emptySpace :: LocationCard EmptySpace
emptySpace =
  locationWith EmptySpace Cards.emptySpace 0 (Static 0)
    $ connectsToL
    .~ adjacentLocations

instance HasModifiersFor EmptySpace where
  getModifiersFor (EmptySpace a) = modifySelf a [IsEmptySpace]

instance HasAbilities EmptySpace where
  getAbilities (EmptySpace l) =
    [ basicAbility
        $ restricted
          l
          102
          ( CanMoveTo (IncludeEmptySpace $ LocationWithId l.id)
              <> OnLocation (IncludeEmptySpace $ accessibleTo ForMovement l)
              <> exists
                (You <> can.move <> noModifier (CannotEnter l.id) <> InvestigatorWithModifier CanEnterEmptySpace)
          )
        $ ActionAbility [#move] moveCost
    ]
   where
    moveCost =
      if l.revealed
        then ActionCost 1
        else ActionCost 1 <> NonBlankedCost (CostToEnterUnrevealed $ locationCostToEnterUnrevealed l)

instance RunMessage EmptySpace where
  runMessage msg l@(EmptySpace attrs) = runQueueT $ case msg of
    Do (PlaceCosmos _ (is attrs -> True) cloc) -> do
      handleCosmos attrs cloc
      pure l
    _ -> EmptySpace <$> liftRunMessage msg attrs
