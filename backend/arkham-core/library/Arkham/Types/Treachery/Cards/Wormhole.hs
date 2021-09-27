module Arkham.Types.Treachery.Cards.Wormhole
  ( wormhole
  , Wormhole(..)
  ) where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype Wormhole = Wormhole TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wormhole :: TreacheryCard Wormhole
wormhole = treachery Wormhole Cards.wormhole

instance TreacheryRunner env => RunMessage env Wormhole where
  runMessage msg t@(Wormhole attrs) = case msg of
    Revelation iid source | isSource attrs source -> t <$ push
      (DiscardEncounterUntilFirst
        (ProxySource source (InvestigatorSource iid))
        (CardWithType LocationType)
      )
    RequestedEncounterCard (ProxySource source (InvestigatorSource iid)) mcard
      | isSource attrs source -> t <$ case mcard of
        Nothing -> pure ()
        Just card -> do
          let locationId = LocationId $ toCardId card
          pushAll
            [ InvestigatorDrewEncounterCard iid card
            , MoveTo (toSource attrs) iid locationId
            ]
    _ -> Wormhole <$> runMessage msg attrs
