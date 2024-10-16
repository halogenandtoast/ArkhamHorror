module Arkham.Location.Cards.CliffsideRoad_b (cliffsideRoad_b, CliffsideRoad_b (..)) where

import Arkham.Ability
import Arkham.Asset.Types (Field (AssetDriver))
import Arkham.Direction
import Arkham.Helpers.Location (getLocationOf)
import Arkham.Helpers.SkillTest (getSkillTestTarget)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.Placement
import Arkham.Projection
import Arkham.Scenarios.HorrorInHighGear.Helpers

newtype CliffsideRoad_b = CliffsideRoad_b LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cliffsideRoad_b :: LocationCard CliffsideRoad_b
cliffsideRoad_b =
  locationWith CliffsideRoad_b Cards.cliffsideRoad_b 1 (PerPlayer 3)
    $ connectsToL
    .~ setFromList [LeftOf, RightOf]

instance HasAbilities CliffsideRoad_b where
  getAbilities (CliffsideRoad_b a) =
    extendRevealed
      a
      [ mkAbility a 1 $ SilentForcedAbility $ RevealLocation #after Anyone (be a)
      , mkAbility a 2 $ forced $ VehicleLeaves #when (notSeenVehicle a) (be a)
      ]

instance RunMessage CliffsideRoad_b where
  runMessage msg l@(CliffsideRoad_b attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      road 1 attrs
      pure l
    UseCardAbility _iid (isSource attrs -> True) 2 (getLeavingVehicle -> vehicle) _ -> do
      n <- handleVehicleLeaves vehicle attrs.id 2
      when (n > 0) do
        driver <- fieldJust AssetDriver vehicle
        sid <- getRandom
        chooseOneM driver do
          for_ [#willpower, #agility] \sType -> do
            labeled ("Test " <> format sType <> "(" <> tshow n <> ")")
              $ beginSkillTest sid driver (attrs.ability 2) vehicle sType (Fixed n)
      pure . CliffsideRoad_b $ attrs & globalMetaL %~ sawVehicle vehicle
    FailedThisSkillTest _iid (isAbilitySource attrs 2 -> True) -> do
      getSkillTestTarget >>= \case
        Just (AssetTarget aid) ->
          getLocationOf aid >>= \case
            Nothing -> error "missing location"
            Just lid -> do
              passengers <- select $ InVehicleMatching (AssetWithId aid)
              for_ passengers \iid -> place iid (AtLocation lid)
              removeFromGame aid
              for_ passengers \iid -> assignDamage iid (attrs.ability 2) 10
        _ -> error "wrong target"
      pure l
    _ -> CliffsideRoad_b <$> liftRunMessage msg attrs
