module Arkham.Treachery.Cards.ShadowedDealingsInTheDark (shadowedDealingsInTheDark) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Cards.Shadowed
import Arkham.Treachery.Import.Lifted

newtype ShadowedDealingsInTheDark = ShadowedDealingsInTheDark Shadowed
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor, HasAbilities)

shadowedDealingsInTheDark :: TreacheryCard ShadowedDealingsInTheDark
shadowedDealingsInTheDark = treachery (ShadowedDealingsInTheDark . Shadowed) Cards.shadowedDealingsInTheDark

instance RunMessage ShadowedDealingsInTheDark where
  runMessage msg (ShadowedDealingsInTheDark inner) =
    ShadowedDealingsInTheDark <$> runMessage msg inner
