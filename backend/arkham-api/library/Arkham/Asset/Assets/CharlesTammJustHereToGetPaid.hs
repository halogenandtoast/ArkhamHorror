module Arkham.Asset.Assets.CharlesTammJustHereToGetPaid (charlesTammJustHereToGetPaid) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Location.CardDefs.TracesToNowhere qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Placement
import Arkham.Scenarios.TracesToNowhere.Helpers

newtype CharlesTammJustHereToGetPaid = CharlesTammJustHereToGetPaid AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

charlesTammJustHereToGetPaid :: AssetCard CharlesTammJustHereToGetPaid
charlesTammJustHereToGetPaid =
  ally CharlesTammJustHereToGetPaid Cards.charlesTammJustHereToGetPaid (2, 2)

-- | "Investigators at Charles Tamm's location cannot spend clues."
instance HasModifiersFor CharlesTammJustHereToGetPaid where
  getModifiersFor (CharlesTammJustHereToGetPaid a) =
    modifySelect a (InvestigatorAt $ locationWithAsset a) [CannotSpendClues]

instance HasAbilities CharlesTammJustHereToGetPaid where
  getAbilities (CharlesTammJustHereToGetPaid a) =
    [restricted a 1 OnSameLocation parleyAction_]

instance RunMessage CharlesTammJustHereToGetPaid where
  runMessage msg a@(CharlesTammJustHereToGetPaid attrs) = runQueueT $ scenarioI18n $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      selectForMaybeM (locationIs Locations.steersOffice) $ place attrs . AtLocation
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      codexEntry "codex.charlesTamm"
      pure a
    _ -> CharlesTammJustHereToGetPaid <$> liftRunMessage msg attrs
