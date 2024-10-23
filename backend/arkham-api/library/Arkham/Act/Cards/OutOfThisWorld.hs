module Arkham.Act.Cards.OutOfThisWorld (OutOfThisWorld (..), outOfThisWorld) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Card
import Arkham.Location.Cards qualified as Locations
import Arkham.Message.Lifted.Choose

newtype OutOfThisWorld = OutOfThisWorld ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

outOfThisWorld :: ActCard OutOfThisWorld
outOfThisWorld = act (1, A) OutOfThisWorld Cards.outOfThisWorld (groupClueCost (PerPlayer 2))

instance HasAbilities OutOfThisWorld where
  getAbilities (OutOfThisWorld x) = extend x [mkAbility x 1 actionAbility]

instance RunMessage OutOfThisWorld where
  runMessage msg a@(OutOfThisWorld attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      placeSetAsideLocation_ Locations.theEdgeOfTheUniverse
      advanceActDeck attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ DiscardTopOfEncounterDeck iid 3 (toSource attrs) (Just $ toTarget attrs)
      pure a
    DiscardedTopOfEncounterDeck iid cards _ (isTarget attrs -> True) -> do
      let locationCards = filterLocations cards
      focusCards (map toCard cards) \unfocus -> do
        chooseOneM iid do
          when (null locationCards) $ labeled "No locations found" $ push unfocus
          targets locationCards \location -> pushAll [unfocus, ResolveRevelation iid (toCard location)]
      pure a
    _ -> OutOfThisWorld <$> liftRunMessage msg attrs
