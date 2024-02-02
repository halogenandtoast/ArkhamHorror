module Arkham.Treachery.Cards.Wormhole (
  wormhole,
  Wormhole (..),
) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Matcher
import Arkham.Movement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype Wormhole = Wormhole TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

wormhole :: TreacheryCard Wormhole
wormhole = treachery Wormhole Cards.wormhole

instance RunMessage Wormhole where
  runMessage msg t@(Wormhole attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      push
        $ DiscardUntilFirst
          iid
          source
          Deck.EncounterDeck
          (BasicCardMatch $ CardWithType LocationType)
      pure t
    RequestedEncounterCard (isSource attrs -> True) (Just iid) (Just card) ->
      do
        pushAll
          [ InvestigatorDrewEncounterCard iid card
          , MoveTo $ moveToMatch (toSource attrs) iid (LocationWithCardId $ toCardId card)
          ]
        pure t
    _ -> Wormhole <$> runMessage msg attrs
