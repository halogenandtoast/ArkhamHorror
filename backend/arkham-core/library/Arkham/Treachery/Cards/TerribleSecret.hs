module Arkham.Treachery.Cards.TerribleSecret (
  terribleSecret,
  TerribleSecret (..),
)
where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype TerribleSecret = TerribleSecret TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

terribleSecret :: TreacheryCard TerribleSecret
terribleSecret = treachery TerribleSecret Cards.terribleSecret

-- TODO: Cannot cancel
instance RunMessage TerribleSecret where
  runMessage msg t@(TerribleSecret attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      cardsUnderneath <- field InvestigatorCardsUnderneath iid
      if null cardsUnderneath
        then push $ ShuffleIntoDeck (Deck.InvestigatorDeck iid) (toTarget attrs)
        else do
          player <- getPlayer iid
          pushAll
            [ FocusCards cardsUnderneath
            , chooseUpToN
                player
                (length cardsUnderneath)
                "Keep Remaining Cards"
                [ targetLabel (toCardId c) [AddToDiscard iid c]
                | pc <- cardsUnderneath
                , c <- maybeToList (preview _PlayerCard pc)
                ]
            , UnfocusCards
            ]
      pure t
    AfterRevelation iid tid | tid == toId attrs -> do
      cardsUnderneath <- field InvestigatorCardsUnderneath iid
      push $ InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 (length cardsUnderneath)
      pure t
    _ -> TerribleSecret <$> runMessage msg attrs
