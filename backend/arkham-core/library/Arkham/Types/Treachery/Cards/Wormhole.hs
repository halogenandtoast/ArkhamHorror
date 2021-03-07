module Arkham.Types.Treachery.Cards.Wormhole
  ( wormhole
  , Wormhole(..)
  )
where

import Arkham.Prelude

import Arkham.Types.Card
import Arkham.Types.Card.EncounterCardMatcher
import Arkham.Types.Classes
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner
import Arkham.Types.TreacheryId

newtype Wormhole = Wormhole TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wormhole :: TreacheryId -> a -> Wormhole
wormhole uuid _ = Wormhole $ baseAttrs uuid "02332"

instance HasModifiersFor env Wormhole where
  getModifiersFor = noModifiersFor

instance HasActions env Wormhole where
  getActions i window (Wormhole attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env Wormhole where
  runMessage msg t@(Wormhole attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> t <$ unshiftMessages
      [ DiscardEncounterUntilFirst
        (ProxySource source (InvestigatorSource iid))
        (EncounterCardMatchByType (LocationType, Nothing))
      , Discard (toTarget attrs)
      ]
    RequestedEncounterCard (ProxySource source (InvestigatorSource iid)) mcard
      | isSource attrs source -> t <$ case mcard of
        Nothing -> pure ()
        Just card -> do
          let locationId = LocationId $ getCardId card
          unshiftMessages
            [InvestigatorDrewEncounterCard iid card, MoveTo iid locationId]
    _ -> Wormhole <$> runMessage msg attrs
