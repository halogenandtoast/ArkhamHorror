module Arkham.Homebrew.CircusExMortis.Assets.CarrieDykstra (carrieDykstra) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Location (getConnectedLocations, getLocationOf)
import Arkham.Homebrew.CircusExMortis.CardDefs.Assets qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move (moveTo)
import Arkham.Message.Lifted.Placement (place)
import Arkham.Placement
import Arkham.Trait (Trait (Train))

newtype CarrieDykstra = CarrieDykstra AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

carrieDykstra :: AssetCard CarrieDykstra
carrieDykstra = asset CarrieDykstra Cards.carrieDykstra

instance HasAbilities CarrieDykstra where
  getAbilities (CarrieDykstra attrs) =
    mkAbility attrs 1 (freeTrigger (exhaust attrs))
      : case attrs.placement of
        AtLocation lid ->
          [groupLimit PerRound $ mkAbility attrs 2 $ freeReaction (Enters #after You (be lid))]
        _ -> []

instance RunMessage CarrieDykstra where
  runMessage msg a@(CarrieDykstra attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      mLoc <- getLocationOf attrs.placement
      for_ mLoc \loc -> do
        conns <- filterM (<=~> LocationWithTrait Train) =<< getConnectedLocations loc
        chooseOrRunOneM iid $ targets conns $ place attrs . AtLocation
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      chooseOneM iid do
        for_ [#combat, #agility] \sType ->
          skillLabeled sType $ beginSkillTest sid iid (attrs.ability 2) attrs sType (Fixed 2)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      mLoc <- getLocationOf attrs.placement
      for_ mLoc \loc -> do
        conns <- getConnectedLocations loc
        chooseOrRunOneM iid $ targets conns (moveTo attrs iid)
      pure a
    _ -> CarrieDykstra <$> liftRunMessage msg attrs
