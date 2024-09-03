module Arkham.Location.Cards.ParlorCar (
  parlorCar,
  ParlorCar (..),
) where

import Arkham.Ability
import Arkham.Classes
import Arkham.Direction
import Arkham.Discover
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (parlorCar)
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Prelude
import Arkham.Projection

newtype ParlorCar = ParlorCar LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

parlorCar :: LocationCard ParlorCar
parlorCar =
  locationWith
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
        pure $ toModifiers l $ CannotInvestigate : [Blocked | not locationRevealed && clueCount > 0]
      Nothing -> pure $ toModifiers l [CannotInvestigate]
  getModifiersFor _ _ = pure []

instance HasAbilities ParlorCar where
  getAbilities (ParlorCar attrs) =
    extendRevealed
      attrs
      [ restrictedAbility
          attrs
          1
          (Here <> CluesOnThis (atLeast 1) <> youExist (InvestigatorCanDiscoverCluesAt YourLocation))
          $ actionAbilityWithCost (ResourceCost 3)
      ]

instance RunMessage ParlorCar where
  runMessage msg l@(ParlorCar attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ Msg.DiscoverClues iid $ discover attrs (attrs.ability 1) 1
      pure l
    _ -> ParlorCar <$> runMessage msg attrs
