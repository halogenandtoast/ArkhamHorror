module Arkham.Location.Cards.DesolateRoad_b (desolateRoad_b, DesolateRoad_b (..)) where

import Arkham.Ability
import Arkham.Deck qualified as Deck
import Arkham.Direction
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.HorrorInHighGear.Helpers
import Arkham.Trait (Trait (Vehicle))

newtype DesolateRoad_b = DesolateRoad_b LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

desolateRoad_b :: LocationCard DesolateRoad_b
desolateRoad_b =
  locationWith DesolateRoad_b Cards.desolateRoad_b 4 (PerPlayer 1)
    $ connectsToL
    .~ setFromList [LeftOf, RightOf]

instance HasAbilities DesolateRoad_b where
  getAbilities (DesolateRoad_b a) =
    extendRevealed
      a
      [ mkAbility a 1 $ SilentForcedAbility $ RevealLocation #after Anyone (be a)
      , mkAbility a 2 $ forced $ VehicleLeaves #when (notSeenVehicle a) (be a)
      ]

instance RunMessage DesolateRoad_b where
  runMessage msg l@(DesolateRoad_b attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      road 1 attrs
      pure l
    UseCardAbility iid (isSource attrs -> True) 2 (getLeavingVehicle -> vehicle) _ -> do
      n <- handleVehicleLeaves vehicle attrs.id 2
      when (n > 0)
        $ discardUntilFirst iid (attrs.ability 2) Deck.EncounterDeck (basic $ #enemy <> withTrait Vehicle)

      pure . DesolateRoad_b $ attrs & globalMetaL %~ sawVehicle vehicle
    RequestedEncounterCard (isAbilitySource attrs 2 -> True) _ (Just card) -> do
      createEnemyAt_ card attrs.id
      pure l
    _ -> DesolateRoad_b <$> liftRunMessage msg attrs
