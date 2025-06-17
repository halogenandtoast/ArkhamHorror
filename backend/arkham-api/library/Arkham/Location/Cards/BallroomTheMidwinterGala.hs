module Arkham.Location.Cards.BallroomTheMidwinterGala (ballroomTheMidwinterGala) where

import Arkham.Ability
import Arkham.Helpers.Asset (assetCanHaveHorrorHealed)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Guest))

newtype BallroomTheMidwinterGala = BallroomTheMidwinterGala LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | 'BallroomTheMidwinterGala' from The Midwinter Gala (#71010).
ballroomTheMidwinterGala :: LocationCard BallroomTheMidwinterGala
ballroomTheMidwinterGala = location BallroomTheMidwinterGala Cards.ballroomTheMidwinterGala 2 (PerPlayer 1)

instance HasAbilities BallroomTheMidwinterGala where
  getAbilities (BallroomTheMidwinterGala a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> exists (assetAt a <> AssetWithTrait Guest <> AssetReady)) parleyAction_

instance RunMessage BallroomTheMidwinterGala where
  runMessage msg l@(BallroomTheMidwinterGala attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assets <- select $ assetAt attrs.id <> AssetWithTrait Guest <> AssetReady
      chooseTargetM iid assets (handleTarget iid attrs)
      pure l
    HandleTargetChoice iid (isSource attrs -> True) (AssetTarget aid) -> do
      exhaustThis aid
      assets <- select $ assetAt attrs.id <> AssetWithTrait Guest <> AssetReady <> not_ (AssetWithId aid)
      chooseTargetM iid assets \aid' -> do
        exhaustThis aid'
        whenM (assetCanHaveHorrorHealed (attrs.ability 1) aid) $ healHorror aid (attrs.ability 1) 1
        whenM (assetCanHaveHorrorHealed (attrs.ability 1) aid') $ healHorror aid' (attrs.ability 1) 1
      pure l
    _ -> BallroomTheMidwinterGala <$> liftRunMessage msg attrs
