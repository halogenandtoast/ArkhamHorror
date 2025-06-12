module Arkham.Act.Cards.TheGuardiansInquiry (theGuardiansInquiry) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Matcher
import Arkham.Placement

newtype TheGuardiansInquiry = TheGuardiansInquiry ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theGuardiansInquiry :: ActCard TheGuardiansInquiry
theGuardiansInquiry =
  act (1, E) TheGuardiansInquiry Cards.theGuardiansInquiry
    $ Just
    $ GroupClueCost (PerPlayer 1)
    $ LocationWithTitle "Northside"

instance RunMessage TheGuardiansInquiry where
  runMessage msg a@(TheGuardiansInquiry attrs) = runQueueT $ case msg of
    AdvanceAct (isSide F attrs -> True) _ _ -> do
      curiositieShoppe <- selectJust $ LocationWithTitle "Curiositie Shoppe"
      createAssetAt_ Assets.mariaDeSilva (AtLocation curiositieShoppe)
      advanceActDeck attrs
      pure a
    _ -> TheGuardiansInquiry <$> liftRunMessage msg attrs
