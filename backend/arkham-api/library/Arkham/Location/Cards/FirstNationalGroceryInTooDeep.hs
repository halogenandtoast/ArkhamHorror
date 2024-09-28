module Arkham.Location.Cards.FirstNationalGroceryInTooDeep (
  firstNationalGroceryInTooDeep,
  FirstNationalGroceryInTooDeep (..),
)
where

import Arkham.Ability
import Arkham.Helpers.Investigator (getSkillValue)
import Arkham.Id
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers (connectsToAdjacent)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.ScenarioLogKey
import Arkham.Scenarios.InTooDeep.Helpers
import Arkham.Window (Window)
import Arkham.Window qualified as Window

newtype FirstNationalGroceryInTooDeep = FirstNationalGroceryInTooDeep LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

firstNationalGroceryInTooDeep :: LocationCard FirstNationalGroceryInTooDeep
firstNationalGroceryInTooDeep =
  locationWith
    FirstNationalGroceryInTooDeep
    Cards.firstNationalGroceryInTooDeep
    3
    (PerPlayer 2)
    connectsToAdjacent

instance HasAbilities FirstNationalGroceryInTooDeep where
  getAbilities (FirstNationalGroceryInTooDeep a) =
    extendRevealed
      a
      [ mkAbility a 1 $ forced $ Moves #after You AnySource (ConnectedTo $ be a) (be a)
      , skillTestAbility $ restricted a 2 Here parleyAction_
      ]

getMovedFrom :: [Window] -> LocationId
getMovedFrom [] = error "getLocationsInMove: not a Moves event"
getMovedFrom ((Window.windowType -> Window.Moves _ _ (Just from) _) : _) = from
getMovedFrom (_ : rest) = getMovedFrom rest

instance RunMessage FirstNationalGroceryInTooDeep where
  runMessage msg l@(FirstNationalGroceryInTooDeep attrs) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 (getMovedFrom -> from) _ -> do
      push $ ScenarioCountIncrementBy (Barriers attrs.id from) 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      choices <- mins <$> traverse (traverseToSnd (`getSkillValue` iid)) [minBound .. maxBound]
      sid <- getRandom
      chooseOrRunOneM iid do
        for_ choices \skill ->
          skillLabeled skill $ beginSkillTest sid iid (attrs.ability 2) iid skill (Fixed 2)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      removeBarrierBetweenConnected iid attrs.id
      pure l
    _ -> FirstNationalGroceryInTooDeep <$> liftRunMessage msg attrs
