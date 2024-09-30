module Arkham.Treachery.Cards.Stowaway (stowaway, Stowaway (..)) where

import Arkham.Asset.Types (Field (..))
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.Modifier
import Arkham.Placement
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Stowaway = Stowaway TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stowaway :: TreacheryCard Stowaway
stowaway = treachery Stowaway Cards.stowaway

instance RunMessage Stowaway where
  runMessage msg t@(Stowaway attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      inVehicle <- selectWithField InvestigatorPlacement $ InVehicleMatching AnyAsset
      if null inVehicle
        then gainSurge attrs
        else for_ inVehicle \(iid, placement) -> chooseOneM iid do
          labeled "Take 1 damage and 1 horror" $ assignDamageAndHorror iid attrs 1 1
          labeled "Leave the vehicle. You cannot enter that vehicle for the remainder of the round" do
            case placement of
              InVehicle asset -> do
                mloc <- field AssetLocation asset
                for_ mloc \loc -> place iid (AtLocation loc)
                roundModifier attrs iid (CannotEnterVehicle $ AssetWithId asset)
              _ -> error "invalid placement"

      pure t
    _ -> Stowaway <$> liftRunMessage msg attrs
