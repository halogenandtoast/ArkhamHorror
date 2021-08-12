module Arkham.Types.Treachery.Cards.Wormhole
  ( wormhole
  , Wormhole(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype Wormhole = Wormhole TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasActions)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wormhole :: TreacheryCard Wormhole
wormhole = treachery Wormhole Cards.wormhole

instance TreacheryRunner env => RunMessage env Wormhole where
  runMessage msg t@(Wormhole attrs) = case msg of
    Revelation iid source | isSource attrs source -> t <$ pushAll
      [ DiscardEncounterUntilFirst
        (ProxySource source (InvestigatorSource iid))
        (CardWithType LocationType)
      , Discard (toTarget attrs)
      ]
    RequestedEncounterCard (ProxySource source (InvestigatorSource iid)) mcard
      | isSource attrs source -> t <$ case mcard of
        Nothing -> pure ()
        Just card -> do
          let locationId = LocationId $ toCardId card
          pushAll
            [ InvestigatorDrewEncounterCard iid card
            , MoveTo iid locationId
            , MovedBy iid (toSource attrs)
            ]
    _ -> Wormhole <$> runMessage msg attrs
