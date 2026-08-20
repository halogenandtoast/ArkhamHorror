module Arkham.Location.Cards.TheInnsmouthConspiracy.InTooDeep.InnsmouthSquare (
  innsmouthSquare,
  InnsmouthSquare (..),
)
where

import Arkham.Ability
import Arkham.Enemy.CardDefs.TheInnsmouthConspiracy.InTooDeep qualified as Enemies
import Arkham.Enemy.Creation
import Arkham.Helpers.Modifiers (ModifierType (..), getModifiers)
import Arkham.Location.CardDefs.TheInnsmouthConspiracy.InTooDeep qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.ScenarioLogKey
import Arkham.Scenarios.TheInnsmouthConspiracy.InTooDeep.Helpers

newtype InnsmouthSquare = InnsmouthSquare LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

innsmouthSquare :: LocationCard InnsmouthSquare
innsmouthSquare =
  locationWith
    InnsmouthSquare
    Cards.innsmouthSquare
    4
    (PerPlayer 1)
    connectsToAdjacent

instance HasAbilities InnsmouthSquare where
  getAbilities (InnsmouthSquare a) =
    extendRevealed
      a
      [ mkAbility a 1 $ forced $ RevealLocation #after Anyone (be a)
      , restricted a 2 (Here <> thisIs a LocationWithAdjacentBarrier) parleyAction_
      , groupLimit PerGame
          $ restricted a 3 (Here <> HasCalculation (InvestigatorKeyCountCalculation Anyone) (atLeast 2))
          $ FastAbility' Free #parley
      ]

instance RunMessage InnsmouthSquare where
  runMessage msg l@(InnsmouthSquare attrs) = runQueueT $ case msg of
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
      others <- select $ connectedTo (be attrs) <> not_ (LocationWithId lid)
      chooseTargetM iid others \choice -> do
        push $ ScenarioCountDecrementBy (Barriers attrs.id lid) 1
        push $ ScenarioCountIncrementBy (Barriers attrs.id choice) 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      flashback iid Flashback5
      pure l
    _ -> InnsmouthSquare <$> liftRunMessage msg attrs
