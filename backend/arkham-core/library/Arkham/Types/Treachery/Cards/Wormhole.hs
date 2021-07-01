module Arkham.Types.Treachery.Cards.Wormhole
  ( wormhole
  , Wormhole(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype Wormhole = Wormhole TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wormhole :: TreacheryCard Wormhole
wormhole = treachery Wormhole Cards.wormhole

instance HasModifiersFor env Wormhole where
  getModifiersFor = noModifiersFor

instance HasActions env Wormhole where
  getActions i window (Wormhole attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env Wormhole where
  runMessage msg t@(Wormhole attrs) = case msg of
    Revelation iid source | isSource attrs source -> t <$ unshiftMessages
      [ DiscardEncounterUntilFirst
        (ProxySource source (InvestigatorSource iid))
        (CardMatchByType (LocationType, mempty))
      , Discard (toTarget attrs)
      ]
    RequestedEncounterCard (ProxySource source (InvestigatorSource iid)) mcard
      | isSource attrs source -> t <$ case mcard of
        Nothing -> pure ()
        Just card -> do
          let locationId = LocationId $ toCardId card
          unshiftMessages
            [InvestigatorDrewEncounterCard iid card, MoveTo iid locationId]
    _ -> Wormhole <$> runMessage msg attrs
