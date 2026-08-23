module Arkham.Treachery.Cards.ChildrenOfBlood.BloodMoney.SanguineRebirth (sanguineRebirth) where

import Arkham.Treachery.CardDefs.ChildrenOfBlood.BloodMoney qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype SanguineRebirth = SanguineRebirth TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sanguineRebirth :: TreacheryCard SanguineRebirth
sanguineRebirth = treachery SanguineRebirth Cards.sanguineRebirth

instance RunMessage SanguineRebirth where
  runMessage msg (SanguineRebirth attrs) = SanguineRebirth <$> runMessage msg attrs
