module Arkham.Treachery.Cards.Outbreak
  ( outbreak
  , Outbreak(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype Outbreak = Outbreak TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

outbreak :: TreacheryCard Outbreak
outbreak = treachery Outbreak Cards.outbreak

instance RunMessage Outbreak where
  runMessage msg t@(Outbreak attrs) = case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> Outbreak <$> runMessage msg attrs
