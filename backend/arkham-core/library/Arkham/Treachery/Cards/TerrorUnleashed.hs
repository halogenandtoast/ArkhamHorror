module Arkham.Treachery.Cards.TerrorUnleashed
  ( terrorUnleashed
  , TerrorUnleashed(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype TerrorUnleashed = TerrorUnleashed TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

terrorUnleashed :: TreacheryCard TerrorUnleashed
terrorUnleashed = treachery TerrorUnleashed Cards.terrorUnleashed

instance RunMessage TerrorUnleashed where
  runMessage msg t@(TerrorUnleashed attrs) = case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> TerrorUnleashed <$> runMessage msg attrs
