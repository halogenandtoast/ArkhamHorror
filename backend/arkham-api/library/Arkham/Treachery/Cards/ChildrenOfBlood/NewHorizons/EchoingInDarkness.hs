module Arkham.Treachery.Cards.ChildrenOfBlood.NewHorizons.EchoingInDarkness (echoingInDarkness) where

import Arkham.Treachery.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype EchoingInDarkness = EchoingInDarkness TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

echoingInDarkness :: TreacheryCard EchoingInDarkness
echoingInDarkness = treachery EchoingInDarkness Cards.echoingInDarkness

instance RunMessage EchoingInDarkness where
  runMessage msg (EchoingInDarkness attrs) = EchoingInDarkness <$> runMessage msg attrs
