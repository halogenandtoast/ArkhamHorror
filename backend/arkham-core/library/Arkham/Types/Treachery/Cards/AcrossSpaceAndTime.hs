module Arkham.Types.Treachery.Cards.AcrossSpaceAndTime
  ( acrossSpaceAndTime
  , AcrossSpaceAndTime(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards (acrossSpaceAndTime)
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.TreacheryId
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype AcrossSpaceAndTime = AcrossSpaceAndTime TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

acrossSpaceAndTime :: InvestigatorId -> TreacheryId -> AcrossSpaceAndTime
acrossSpaceAndTime = treachery AcrossSpaceAndTime Cards.acrossSpaceAndTime

instance HasModifiersFor env AcrossSpaceAndTime where
  getModifiersFor = noModifiersFor

instance HasActions env AcrossSpaceAndTime where
  getActions i window (AcrossSpaceAndTime attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env AcrossSpaceAndTime where
  runMessage msg t@(AcrossSpaceAndTime attrs) = case msg of
    Revelation iid source | isSource attrs source -> t <$ unshiftMessages
      [DiscardTopOfDeck iid 3 Nothing, Discard (toTarget attrs)]
    _ -> AcrossSpaceAndTime <$> runMessage msg attrs
