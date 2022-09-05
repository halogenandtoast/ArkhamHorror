module Arkham.Location.Cards.ParlorCar
  ( parlorCar
  , ParlorCar(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Direction
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards ( parlorCar )
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection

newtype ParlorCar = ParlorCar LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

parlorCar :: LocationCard ParlorCar
parlorCar = locationWith
  ParlorCar
  Cards.parlorCar
  3
  (PerPlayer 1)
  (connectsToL .~ setFromList [LeftOf, RightOf])

instance HasModifiersFor ParlorCar where
  getModifiersFor target (ParlorCar l@LocationAttrs {..}) | isTarget l target =
    case lookup LeftOf locationDirections of
      Just leftLocation -> do
        clueCount <- field LocationClues leftLocation
        pure
          $ toModifiers l
          $ CannotInvestigate
          : [ Blocked | not locationRevealed && clueCount > 0 ]
      Nothing -> pure $ toModifiers l [CannotInvestigate]
  getModifiersFor _ _ = pure []

instance HasAbilities ParlorCar where
  getAbilities (ParlorCar attrs) =
    withBaseAbilities attrs
      $ [ restrictedAbility
            attrs
            1
            (Here <> CluesOnThis (AtLeast $ Static 1) <> InvestigatorExists
              (You <> InvestigatorCanDiscoverCluesAt YourLocation)
            )
          $ ActionAbility Nothing
          $ Costs [ActionCost 1, ResourceCost 3]
        | locationRevealed attrs
        ]

instance RunMessage ParlorCar where
  runMessage msg l@(ParlorCar attrs@LocationAttrs {..}) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ push (InvestigatorDiscoverClues iid locationId 1 Nothing)
    _ -> ParlorCar <$> runMessage msg attrs
