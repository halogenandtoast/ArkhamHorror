module Arkham.Treachery.Cards.StarsOfHyades
  ( starsOfHyades
  , StarsOfHyades(..)
  ) where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Card
import Arkham.Classes
import Arkham.Message
import Arkham.Target
import Arkham.Treachery.Attrs
import Arkham.Treachery.Runner

newtype StarsOfHyades = StarsOfHyades TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

starsOfHyades :: TreacheryCard StarsOfHyades
starsOfHyades = treachery StarsOfHyades Cards.starsOfHyades

instance TreacheryRunner env => RunMessage StarsOfHyades where
  runMessage msg t@(StarsOfHyades attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      underneathCards <- map unUnderneathCard <$> getList iid
      let events = filter ((== EventType) . toCardType) underneathCards
      t <$ case nonEmpty events of
        Nothing -> push (InvestigatorAssignDamage iid source DamageAny 1 1)
        Just targets -> do
          deckSize <- length <$> getList @DeckCard iid
          discardedEvent <- sample targets
          pushAll
            (chooseOne
                iid
                [RemoveFromGame (CardIdTarget $ toCardId discardedEvent)]
            : [ ShuffleIntoDeck iid (toTarget attrs) | deckSize >= 5 ]
            )
    _ -> StarsOfHyades <$> runMessage msg attrs
