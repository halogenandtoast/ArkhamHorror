module Arkham.Treachery.Cards.Indebted (indebted) where

import Arkham.Helpers.Modifiers (modifiedWith_)
import Arkham.Modifier
import Arkham.Treachery.CardDefs.TheDunwichLegacy qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Indebted = Indebted TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

indebted :: TreacheryCard Indebted
indebted = treachery Indebted Cards.indebted

instance HasModifiersFor Indebted where
  getModifiersFor (Indebted attrs) = for_ attrs.inThreatAreaOf \iid ->
    modifiedWith_ attrs iid setActiveDuringSetup [StartingResources (-2)]
