module Arkham.Treachery.Cards.TheMidwinterGala.ViolentCommands (violentCommands) where

import Arkham.Treachery.CardDefs.TheMidwinterGala qualified as Cards
import Arkham.Treachery.Cards.ReturnToTheDunwichLegacy.ErraticFear.ViolentCommands qualified as Base
import Arkham.Treachery.Import.Lifted

newtype ViolentCommands = ViolentCommands Base.ViolentCommands
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities, HasModifiersFor)

violentCommands :: TreacheryCard ViolentCommands
violentCommands =
  treachery
    (ViolentCommands . Base.ViolentCommands)
    Cards.violentCommands

instance RunMessage ViolentCommands where
  runMessage msg (ViolentCommands inner) =
    ViolentCommands <$> runMessage msg inner
