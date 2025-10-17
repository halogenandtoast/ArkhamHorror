module Arkham.Location.Cards.CatedralMetropolitana (catedralMetropolitana) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype CatedralMetropolitana = CatedralMetropolitana LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

catedralMetropolitana :: LocationCard CatedralMetropolitana
catedralMetropolitana = setLabel "b" $ location CatedralMetropolitana Cards.catedralMetropolitana 5 (Static 1)

instance HasAbilities CatedralMetropolitana where
  getAbilities (CatedralMetropolitana attrs) =
    extendRevealed attrs []

instance RunMessage CatedralMetropolitana where
  runMessage msg (CatedralMetropolitana attrs) = runQueueT $ case msg of
    _ -> CatedralMetropolitana <$> liftRunMessage msg attrs
