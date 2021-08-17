module Arkham.Types.Treachery.Cards.StarsOfHyades
  ( starsOfHyades
  , StarsOfHyades(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs

newtype StarsOfHyades = StarsOfHyades TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

starsOfHyades :: TreacheryCard StarsOfHyades
starsOfHyades = treachery StarsOfHyades Cards.starsOfHyades

instance HasModifiersFor env StarsOfHyades

instance HasAbilities env StarsOfHyades where
  getAbilities i window (StarsOfHyades attrs) = getAbilities i window attrs

instance
  ( HasList UnderneathCard env InvestigatorId
  , HasList DeckCard env InvestigatorId
  )
  => RunMessage env StarsOfHyades where
  runMessage msg t@(StarsOfHyades attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      underneathCards <- map unUnderneathCard <$> getList iid
      let events = filter ((== EventType) . toCardType) underneathCards
      t <$ case nonEmpty events of
        Nothing -> pushAll
          [ InvestigatorAssignDamage iid (toSource attrs) DamageAny 1 1
          , Discard $ toTarget attrs
          ]
        Just targets -> do
          deckSize <- length <$> getList @DeckCard iid
          let
            revelationMsg = if deckSize >= 5
              then ShuffleIntoDeck iid (toTarget attrs)
              else Discard $ toTarget attrs
          discardedEvent <- sample targets
          pushAll
            (chooseOne
                iid
                [RemoveFromGame (CardIdTarget $ toCardId discardedEvent)]
            : [revelationMsg]
            )
    _ -> StarsOfHyades <$> runMessage msg attrs
