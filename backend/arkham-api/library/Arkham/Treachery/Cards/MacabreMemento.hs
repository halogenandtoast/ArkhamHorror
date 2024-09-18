{-# LANGUAGE ImplicitParams #-}

module Arkham.Treachery.Cards.MacabreMemento (macabreMemento, MacabreMemento (..)) where

import Arkham.Script
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted hiding (failOnReveal, pushAll)

newtype MacabreMemento = MacabreMemento {attrs :: TreacheryAttrs}
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

macabreMemento :: TreacheryCard MacabreMemento
macabreMemento = treachery MacabreMemento Cards.macabreMemento

instance RunMessage MacabreMemento where
  runMessage = script do
    revelation $ skillTest #willpower (Fixed 3) do
      failOnReveal #cultist
      onFail $ you `takeHorror` 2
