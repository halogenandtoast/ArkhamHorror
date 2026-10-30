-- Scaffold: printed card abilities are not implemented yet.
module Arkham.Homebrew.NightOfVespers.Treacheries.Kenophobia (kenophobia) where

import Arkham.Homebrew.NightOfVespers.CardDefs.Treacheries qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Kenophobia = Kenophobia TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

kenophobia :: TreacheryCard Kenophobia
kenophobia = treachery Kenophobia Cards.kenophobia

instance RunMessage Kenophobia where
  runMessage msg (Kenophobia attrs) = Kenophobia <$> runMessage msg attrs
