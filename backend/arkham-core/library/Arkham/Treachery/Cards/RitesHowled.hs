module Arkham.Treachery.Cards.RitesHowled
  ( ritesHowled
  , RitesHowled(..)
  ) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Game.Helpers
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype RitesHowled = RitesHowled TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ritesHowled :: TreacheryCard RitesHowled
ritesHowled = treachery RitesHowled Cards.ritesHowled

instance RunMessage RitesHowled where
  runMessage msg t@(RitesHowled attrs) = case msg of
    Revelation _iid source | isSource attrs source -> do
      investigatorIds <- getInvestigatorIds
      t <$ pushAll
        ([ DiscardTopOfDeck iid 3 (Just $ toTarget attrs)
         | iid <- investigatorIds
         ]
        <> [Discard (toSource attrs) $ toTarget attrs]
        )
    DiscardedTopOfDeck iid _cards target | isTarget attrs target -> do
      isAltered <-
        selectAny $ LocationWithTrait Altered <> LocationWithInvestigator
          (InvestigatorWithId iid)
      t <$ when
        isAltered
        (do
          discardPile <- field InvestigatorDiscard iid
          push $ ShuffleCardsIntoDeck
            (Deck.InvestigatorDeck iid)
            (map PlayerCard $ filter (isJust . cdCardSubType . toCardDef) discardPile)
        )
    _ -> RitesHowled <$> runMessage msg attrs
