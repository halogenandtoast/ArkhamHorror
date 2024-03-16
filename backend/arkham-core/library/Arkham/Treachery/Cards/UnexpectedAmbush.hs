module Arkham.Treachery.Cards.UnexpectedAmbush
  ( unexpectedAmbush
  , UnexpectedAmbush(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype UnexpectedAmbush = UnexpectedAmbush TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unexpectedAmbush :: TreacheryCard UnexpectedAmbush
unexpectedAmbush = treachery UnexpectedAmbush Cards.unexpectedAmbush

instance RunMessage UnexpectedAmbush where
  runMessage msg t@(UnexpectedAmbush attrs) = case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> UnexpectedAmbush <$> runMessage msg attrs
