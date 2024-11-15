module Arkham.Asset.Assets.DrAmyKenslerProfessorOfBiology (
  drAmyKenslerProfessorOfBiology,
  DrAmyKenslerProfessorOfBiology (..),
)
where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype DrAmyKenslerProfessorOfBiology = DrAmyKenslerProfessorOfBiology AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drAmyKenslerProfessorOfBiology :: AssetCard DrAmyKenslerProfessorOfBiology
drAmyKenslerProfessorOfBiology = allyWith DrAmyKenslerProfessorOfBiology Cards.drAmyKenslerProfessorOfBiology (2, 4) noSlots

instance RunMessage DrAmyKenslerProfessorOfBiology where
  runMessage msg (DrAmyKenslerProfessorOfBiology attrs) = runQueueT $ case msg of
    _ -> DrAmyKenslerProfessorOfBiology <$> liftRunMessage msg attrs
