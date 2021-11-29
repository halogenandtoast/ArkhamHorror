module Arkham.Types.Treachery.Cards.DeadlyFate
  ( deadlyFate
  , DeadlyFate(..)
  ) where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype DeadlyFate = DeadlyFate TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deadlyFate :: TreacheryCard DeadlyFate
deadlyFate = treachery DeadlyFate Cards.deadlyFate

instance TreacheryRunner env => RunMessage env DeadlyFate where
  runMessage msg t@(DeadlyFate attrs) = case msg of
    Revelation _iid source | isSource attrs source ->
      t <$ push (Discard $ toTarget attrs)
    _ -> DeadlyFate <$> runMessage msg attrs
