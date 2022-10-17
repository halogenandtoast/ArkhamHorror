module Arkham.Treachery.Cards.Wormhole
  ( wormhole
  , Wormhole(..)
  ) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype Wormhole = Wormhole TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wormhole :: TreacheryCard Wormhole
wormhole = treachery Wormhole Cards.wormhole

instance RunMessage Wormhole where
  runMessage msg t@(Wormhole attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      push $ DiscardEncounterUntilFirst
        source
        (Just iid)
        (CardWithType LocationType)
      pure t
    RequestedEncounterCard (isSource attrs -> True) (Just iid) (Just card) ->
      do
        let locationId = LocationId $ toCardId card
        pushAll
          [ InvestigatorDrewEncounterCard iid card
          , MoveTo (toSource attrs) iid locationId
          ]
        pure t
    _ -> Wormhole <$> runMessage msg attrs
