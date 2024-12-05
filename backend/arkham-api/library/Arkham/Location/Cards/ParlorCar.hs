module Arkham.Location.Cards.ParlorCar (parlorCar, ParlorCar (..)) where

import Arkham.Ability
import Arkham.Direction
import Arkham.Discover
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (parlorCar)
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message qualified as Msg

newtype ParlorCar = ParlorCar LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

parlorCar :: LocationCard ParlorCar
parlorCar =
  locationWith ParlorCar Cards.parlorCar 3 (PerPlayer 1)
    $ connectsToL
    .~ setFromList [LeftOf, RightOf]

instance HasModifiersFor ParlorCar where
  getModifiersFor (ParlorCar l) = do
    blocked <- selectAny $ leftOf l <> LocationWithAnyClues
    modifySelf l $ CannotInvestigate : [Blocked | l.unrevealed && blocked]

instance HasAbilities ParlorCar where
  getAbilities (ParlorCar attrs) =
    extendRevealed1 attrs
      $ restricted
        attrs
        1
        (Here <> CluesOnThis (atLeast 1) <> youExist (InvestigatorCanDiscoverCluesAt YourLocation))
      $ actionAbilityWithCost (ResourceCost 3)

instance RunMessage ParlorCar where
  runMessage msg l@(ParlorCar attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ Msg.DiscoverClues iid $ discover attrs (attrs.ability 1) 1
      pure l
    _ -> ParlorCar <$> liftRunMessage msg attrs
