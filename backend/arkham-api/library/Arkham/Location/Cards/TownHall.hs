module Arkham.Location.Cards.TownHall (townHall) where

import Arkham.Ability
import Arkham.Card
import Arkham.GameValue
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Projection ()
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype TownHall = TownHall LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

townHall :: LocationCard TownHall
townHall = location TownHall Cards.townHall 4 (PerPlayer 1)

instance HasModifiersFor TownHall where
  getModifiersFor (TownHall a) = do
    modifySelectMap
      a
      (locationIs Cards.downtownFirstBankOfArkham)
      \lid -> [ConnectedToWhen (LocationWithId lid) (LocationWithId $ toId a)]

instance HasAbilities TownHall where
  getAbilities (TownHall a) = extendRevealed1 a $ mkAbility a 1 $ forced $ Enters #after You (be a)

instance RunMessage TownHall where
  runMessage msg l@(TownHall attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      hand <- iid.hand
      chooseAndDiscardCards iid (toAbilitySource attrs 1)
        $ min (length hand - 3) (length hand - count cardIsWeakness hand)
      pure l
    _ -> TownHall <$> liftRunMessage msg attrs
