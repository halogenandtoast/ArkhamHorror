module Arkham.Location.Cards.SecondFloorHall (secondFloorHall) where

import Arkham.Ability
import Arkham.ChaosToken
import Arkham.GameValue
import Arkham.Helpers.Location
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype SecondFloorHall = SecondFloorHall LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

secondFloorHall :: LocationCard SecondFloorHall
secondFloorHall = location SecondFloorHall Cards.secondFloorHall 2 (PerPlayer 1)

instance HasAbilities SecondFloorHall where
  getAbilities (SecondFloorHall a) =
    extendRevealed1 a $ mkAbility a 1 $ freeReaction (Enters #after You $ be a)

instance RunMessage SecondFloorHall where
  runMessage msg l@(SecondFloorHall attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      accessibleLocationIds <- getAccessibleLocations iid (attrs.ability 1)
      chooseTargetM iid accessibleLocationIds $ moveTo attrs iid
      requestChaosTokens iid (attrs.ability 1) 1
      pure l
    RequestedChaosTokens (isAbilitySource attrs 1 -> True) (Just iid) tokens -> do
      resetChaosTokens (attrs.ability 1)
      when (any ((`elem` [Skull, Cultist, Tablet, ElderThing, AutoFail]) . chaosTokenFace) tokens) do
        drawEncounterCard iid (attrs.ability 1)
      pure l
    _ -> SecondFloorHall <$> liftRunMessage msg attrs
