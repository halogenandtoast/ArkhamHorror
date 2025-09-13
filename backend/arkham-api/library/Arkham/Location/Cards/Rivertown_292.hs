module Arkham.Location.Cards.Rivertown_292 (rivertown_292) where

import Arkham.Ability
import Arkham.Card
import Arkham.GameValue
import Arkham.Location.BreachStatus
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.InTheClutchesOfChaos.Helpers

newtype Rivertown_292 = Rivertown_292 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rivertown_292 :: LocationCard Rivertown_292
rivertown_292 = location Rivertown_292 Cards.rivertown_292 3 (Static 0)

instance HasAbilities Rivertown_292 where
  getAbilities (Rivertown_292 a) =
    extendRevealed1 a
      $ restricted a 1 (withBreaches a Here)
      $ actionAbilityWithCost
      $ HandDiscardCost 1 #any

instance RunMessage Rivertown_292 where
  runMessage msg l@(Rivertown_292 attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ (discardedCards -> cards) -> do
      let icons = count (`elem` [#intellect, #wild]) $ concatMap (cdSkills . toCardDef) cards
          n = 1 + min (maybe 0 countBreaches $ locationBreaches attrs) icons
      act <- selectJust AnyAct
      pushAll [RemoveBreaches (toTarget attrs) n, PlaceBreaches (toTarget act) n]
      pure l
    _ -> Rivertown_292 <$> liftRunMessage msg attrs
