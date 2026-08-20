module Arkham.Treachery.Cards.TheCircleUndone.UnionAndDisillusion.PsychopompsSong (psychopompsSong) where

import Arkham.Treachery.CardDefs.TheCircleUndone.UnionAndDisillusion qualified as Cards
import Arkham.Treachery.Cards.TheDunwichLegacy.BloodOnTheAltar.PsychopompsSong qualified as Base
import Arkham.Treachery.Import.Lifted

newtype PsychopompsSong
  = PsychopompsSong Base.PsychopompsSong
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, IsTreachery, HasModifiersFor, HasAbilities)

psychopompsSong :: TreacheryCard PsychopompsSong
psychopompsSong =
  treachery
    (PsychopompsSong . Base.PsychopompsSong)
    Cards.psychopompsSong

instance RunMessage PsychopompsSong where
  runMessage msg (PsychopompsSong inner) =
    PsychopompsSong <$> runMessage msg inner
