module Arkham.Types.Treachery.Cards.StarsOfHyades
  ( starsOfHyades
  , StarsOfHyades(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype StarsOfHyades = StarsOfHyades TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

starsOfHyades :: TreacheryCard StarsOfHyades
starsOfHyades = treachery StarsOfHyades Cards.starsOfHyades

instance HasModifiersFor env StarsOfHyades

instance HasActions env StarsOfHyades where
  getActions i window (StarsOfHyades attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env StarsOfHyades where
  runMessage msg t@(StarsOfHyades attrs) = case msg of
    Revelation _iid source | isSource attrs source ->
      t <$ push (Discard $ toTarget attrs)
    _ -> StarsOfHyades <$> runMessage msg attrs
