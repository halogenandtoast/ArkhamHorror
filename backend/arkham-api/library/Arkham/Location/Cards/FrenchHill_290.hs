module Arkham.Location.Cards.FrenchHill_290 (frenchHill_290) where

import Arkham.Ability
import Arkham.Card
import Arkham.GameValue
import Arkham.Location.BreachStatus hiding (removeBreaches)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.InTheClutchesOfChaos.Helpers

newtype FrenchHill_290 = FrenchHill_290 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

frenchHill_290 :: LocationCard FrenchHill_290
frenchHill_290 = location FrenchHill_290 Cards.frenchHill_290 3 (Static 0)

instance HasAbilities FrenchHill_290 where
  getAbilities (FrenchHill_290 a) =
    extendRevealed1 a
      $ restricted a 1 (withBreaches a Here)
      $ actionAbilityWithCost (HandDiscardCost 1 #any)

instance RunMessage FrenchHill_290 where
  runMessage msg l@(FrenchHill_290 attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ (discardedCards -> (card : _)) -> do
      let icons = count (`elem` [#willpower, #wild]) $ cdSkills $ toCardDef card
          n = 1 + min (maybe 0 countBreaches $ locationBreaches attrs) icons
      act <- selectJust AnyAct
      removeBreaches attrs n
      placeBreaches act n
      pure l
    _ -> FrenchHill_290 <$> liftRunMessage msg attrs
