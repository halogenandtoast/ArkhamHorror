module Arkham.Treachery.Cards.StarsOfHyades
  ( starsOfHyades
  , StarsOfHyades(..)
  ) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Helpers
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Message
import Arkham.Projection
import Arkham.Target
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype StarsOfHyades = StarsOfHyades TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

starsOfHyades :: TreacheryCard StarsOfHyades
starsOfHyades = treachery StarsOfHyades Cards.starsOfHyades

instance RunMessage StarsOfHyades where
  runMessage msg t@(StarsOfHyades attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      events <- fieldMap
        InvestigatorCardsUnderneath
        (filter ((== EventType) . toCardType))
        iid
      t <$ case nonEmpty events of
        Nothing -> push (InvestigatorAssignDamage iid source DamageAny 1 1)
        Just targets -> do
          deckSize <- fieldMap InvestigatorDeck (length . unDeck) iid
          discardedEvent <- sample targets
          pushAll
            (chooseOne
                iid
                [RemoveFromGame (CardTarget discardedEvent)]
            : [ ShuffleIntoDeck (Deck.InvestigatorDeck iid) (toTarget attrs)
              | deckSize >= 5
              ]
            )
    _ -> StarsOfHyades <$> runMessage msg attrs
