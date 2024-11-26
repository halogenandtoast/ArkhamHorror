module Arkham.Location.Cards.InnsmouthSquareInTooDeep (
  innsmouthSquareInTooDeep,
  InnsmouthSquareInTooDeep (..),
)
where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Creation
import Arkham.Helpers.Modifiers (ModifierType (..), getModifiers)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers (connectsToAdjacent)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.ScenarioLogKey
import Arkham.Scenarios.InTooDeep.Helpers

newtype InnsmouthSquareInTooDeep = InnsmouthSquareInTooDeep LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

innsmouthSquareInTooDeep :: LocationCard InnsmouthSquareInTooDeep
innsmouthSquareInTooDeep =
  locationWith
    InnsmouthSquareInTooDeep
    Cards.innsmouthSquareInTooDeep
    4
    (PerPlayer 1)
    connectsToAdjacent

instance HasAbilities InnsmouthSquareInTooDeep where
  getAbilities (InnsmouthSquareInTooDeep a) =
    extendRevealed
      a
      [ mkAbility a 1 $ forced $ RevealLocation #after Anyone (be a)
      , restricted a 2 (Here <> thisIs a LocationWithAdjacentBarrier) parleyAction_
      , groupLimit PerGame
          $ restricted a 3 (Here <> HasCalculation (InvestigatorKeyCountCalculation Anyone) (atLeast 2))
          $ FastAbility' Free [#parley]
      ]

instance RunMessage InnsmouthSquareInTooDeep where
  runMessage msg l@(InnsmouthSquareInTooDeep attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      innsmouthShoggoth <- getSetAsideCard Enemies.innsmouthShoggoth
      createEnemyWith_ innsmouthShoggoth attrs.id createExhausted
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      mods <- getModifiers attrs.id
      let choices = concat [ls | Barricades ls <- mods]
      chooseTargetM iid choices $ handleTarget iid (attrs.ability 2)
      pure l
    HandleTargetChoice iid (isAbilitySource attrs 2 -> True) (LocationTarget lid) -> do
      others <- select $ ConnectedTo (be attrs) <> not_ (LocationWithId lid)
      chooseTargetM iid others \choice -> do
        push $ ScenarioCountDecrementBy (Barriers attrs.id lid) 1
        push $ ScenarioCountIncrementBy (Barriers attrs.id choice) 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      flashback iid Flashback5
      pure l
    _ -> InnsmouthSquareInTooDeep <$> liftRunMessage msg attrs
