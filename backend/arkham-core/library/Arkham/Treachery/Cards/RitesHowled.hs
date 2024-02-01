module Arkham.Treachery.Cards.RitesHowled (
  ritesHowled,
  RitesHowled (..),
) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Game.Helpers
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype RitesHowled = RitesHowled TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

ritesHowled :: TreacheryCard RitesHowled
ritesHowled = treachery RitesHowled Cards.ritesHowled

instance RunMessage RitesHowled where
  runMessage msg t@(RitesHowled attrs) = case msg of
    Revelation _iid source | isSource attrs source -> do
      investigatorIds <- getInvestigatorIds
      pushAll
        $ [ DiscardTopOfDeck iid 3 (toSource attrs) (Just $ toTarget attrs)
          | iid <- investigatorIds
          ]
      pure t
    DiscardedTopOfDeck iid _cards _ target | isTarget attrs target -> do
      isAltered <-
        selectAny $ LocationWithTrait Altered <> locationWithInvestigator iid
      when isAltered $ do
        discardPile <- field InvestigatorDiscard iid
        push
          $ ShuffleCardsIntoDeck
            (Deck.InvestigatorDeck iid)
            ( map PlayerCard
                $ filter (isJust . cdCardSubType . toCardDef) discardPile
            )
      pure t
    _ -> RitesHowled <$> runMessage msg attrs
