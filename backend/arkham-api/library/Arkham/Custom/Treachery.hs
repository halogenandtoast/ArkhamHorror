-- | The runner behind a debug-authored custom treachery. See "Arkham.Custom.Enemy".
module Arkham.Custom.Treachery (CustomTreachery (..), customTreachery) where

import Arkham.Card.CardDef (CardDef)
import Arkham.Treachery.Import.Lifted

newtype CustomTreachery = CustomTreachery TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

customTreachery :: CardDef -> TreacheryCard CustomTreachery
customTreachery = treachery CustomTreachery

instance RunMessage CustomTreachery where
  runMessage msg (CustomTreachery attrs) = CustomTreachery <$> runMessage msg attrs
