module Arkham.Location.Cards.DesolateRoad_a (desolateRoad_a, DesolateRoad_a (..)) where

import Arkham.Ability
import Arkham.Deck qualified as Deck
import Arkham.Direction
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.HorrorInHighGear.Helpers
import Arkham.Trait (Trait (Vehicle))

newtype DesolateRoad_a = DesolateRoad_a LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

desolateRoad_a :: LocationCard DesolateRoad_a
desolateRoad_a =
  locationWith DesolateRoad_a Cards.desolateRoad_a 2 (PerPlayer 2)
    $ connectsToL
    .~ setFromList [LeftOf, RightOf]

instance HasAbilities DesolateRoad_a where
  getAbilities (DesolateRoad_a a) =
    extendRevealed
      a
      [ mkAbility a 1 $ SilentForcedAbility $ RevealLocation #after Anyone (be a)
      , mkAbility a 2 $ forced $ VehicleLeaves #when (notSeenVehicle a) (be a)
      ]

instance RunMessage DesolateRoad_a where
  runMessage msg l@(DesolateRoad_a attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      road 1 attrs
      pure l
    UseCardAbility iid (isSource attrs -> True) 2 (getLeavingVehicle -> vehicle) _ -> do
      n <- handleVehicleLeaves vehicle attrs.id 2
      when (n > 0)
        $ discardUntilFirst iid (attrs.ability 2) Deck.EncounterDeck (basic $ #enemy <> withTrait Vehicle)

      pure . DesolateRoad_a $ attrs & globalMetaL %~ sawVehicle vehicle
    RequestedEncounterCard (isAbilitySource attrs 2 -> True) _ (Just card) -> do
      createEnemyAt_ card attrs.id
      pure l
    _ -> DesolateRoad_a <$> liftRunMessage msg attrs
