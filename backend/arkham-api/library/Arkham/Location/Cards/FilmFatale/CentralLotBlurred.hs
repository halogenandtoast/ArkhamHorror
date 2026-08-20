module Arkham.Location.Cards.FilmFatale.CentralLotBlurred (centralLotBlurred) where

import Arkham.Ability
import Arkham.Location.CardDefs.FilmFatale qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype CentralLotBlurred = CentralLotBlurred LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

centralLotBlurred :: LocationCard CentralLotBlurred
centralLotBlurred = location CentralLotBlurred Cards.centralLotBlurred 4 (PerPlayer 1)

instance HasAbilities CentralLotBlurred where
  getAbilities (CentralLotBlurred a) =
    extendRevealed1 a $ restricted a 1 (thisExists a LocationWithoutClues) $ forced $ RoundEnds #when

instance RunMessage CentralLotBlurred where
  runMessage msg l@(CentralLotBlurred attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      placeCluesUpToClueValue (attrs.ability 1) attrs
      pure l
    _ -> CentralLotBlurred <$> liftRunMessage msg attrs
