module Arkham.Treachery.Cards.WhisperedBargain
  ( whisperedBargain
  , WhisperedBargain(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype WhisperedBargain = WhisperedBargain TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whisperedBargain :: TreacheryCard WhisperedBargain
whisperedBargain = treachery WhisperedBargain Cards.whisperedBargain

instance RunMessage WhisperedBargain where
  runMessage msg t@(WhisperedBargain attrs) = case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> WhisperedBargain <$> runMessage msg attrs
