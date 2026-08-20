module Arkham.Treachery.Cards.TheMidwinterGala.NoxiousFumes (noxiousFumes) where

import Arkham.Treachery.CardDefs.TheMidwinterGala qualified as Cards
import Arkham.Treachery.Cards.MurderAtTheExcelsiorHotel.NoxiousFumes qualified as Base
import Arkham.Treachery.Import.Lifted

newtype NoxiousFumes = NoxiousFumes Base.NoxiousFumes
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities, HasModifiersFor)

noxiousFumes :: TreacheryCard NoxiousFumes
noxiousFumes =
  treachery
    (NoxiousFumes . Base.NoxiousFumes)
    Cards.noxiousFumes

instance RunMessage NoxiousFumes where
  runMessage msg (NoxiousFumes inner) =
    NoxiousFumes <$> runMessage msg inner
