module Arkham.Types.Treachery.Cards.AcrossTimeAndSpace
  ( acrossTimeAndSpace
  , AcrossTimeAndSpace(..)
  ) where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.TreacheryId


import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype AcrossTimeAndSpace = AcrossTimeAndSpace TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

acrossTimeAndSpace :: TreacheryId -> a -> AcrossTimeAndSpace
acrossTimeAndSpace uuid _ = AcrossTimeAndSpace $ baseAttrs uuid "02178"

instance HasModifiersFor env AcrossTimeAndSpace where
  getModifiersFor = noModifiersFor

instance HasActions env AcrossTimeAndSpace where
  getActions i window (AcrossTimeAndSpace attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env AcrossTimeAndSpace where
  runMessage msg t@(AcrossTimeAndSpace attrs) = case msg of
    Revelation iid source | isSource attrs source -> t <$ unshiftMessages
      [DiscardTopOfDeck iid 3 Nothing, Discard (toTarget attrs)]
    _ -> AcrossTimeAndSpace <$> runMessage msg attrs
