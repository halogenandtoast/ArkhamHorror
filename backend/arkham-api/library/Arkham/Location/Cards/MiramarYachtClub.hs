module Arkham.Location.Cards.MiramarYachtClub (miramarYachtClub) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype MiramarYachtClub = MiramarYachtClub LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

miramarYachtClub :: LocationCard MiramarYachtClub
miramarYachtClub = symbolLabel $ location MiramarYachtClub Cards.miramarYachtClub 1 (PerPlayer 1)

instance HasAbilities MiramarYachtClub where
  getAbilities (MiramarYachtClub attrs) =
    extendRevealed attrs []

instance RunMessage MiramarYachtClub where
  runMessage msg (MiramarYachtClub attrs) = runQueueT $ case msg of
    _ -> MiramarYachtClub <$> liftRunMessage msg attrs
