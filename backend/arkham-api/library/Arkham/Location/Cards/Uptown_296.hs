module Arkham.Location.Cards.Uptown_296 (uptown_296) where

import Arkham.Ability
import Arkham.Card
import Arkham.GameValue
import Arkham.Location.BreachStatus hiding (removeBreaches)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.InTheClutchesOfChaos.Helpers

newtype Uptown_296 = Uptown_296 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

uptown_296 :: LocationCard Uptown_296
uptown_296 = location Uptown_296 Cards.uptown_296 3 (Static 0)

instance HasAbilities Uptown_296 where
  getAbilities (Uptown_296 a) =
    extendRevealed1 a
      $ restricted a 1 (withBreaches a Here)
      $ actionAbilityWithCost (HandDiscardCost 1 #any)

instance RunMessage Uptown_296 where
  runMessage msg l@(Uptown_296 attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ (discardedCards -> (card : _)) -> do
      let icons = count (== #agility) $ cdSkills $ toCardDef card
          n = 1 + min (maybe 0 countBreaches $ locationBreaches attrs) icons
      act <- selectJust AnyAct
      removeBreaches attrs n
      placeBreaches act n
      pure l
    _ -> Uptown_296 <$> liftRunMessage msg attrs
