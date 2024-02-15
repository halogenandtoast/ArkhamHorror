module Arkham.Event.Cards.LucidDreaming2 (
  lucidDreaming2,
  LucidDreaming2 (..),
)
where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher

newtype LucidDreaming2 = LucidDreaming2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lucidDreaming2 :: EventCard LucidDreaming2
lucidDreaming2 = event LucidDreaming2 Cards.lucidDreaming2

instance RunMessage LucidDreaming2 where
  runMessage msg e@(LucidDreaming2 attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      cards <-
        select $ oneOf [InHandOf (InvestigatorWithId iid), InPlayAreaOf (InvestigatorWithId iid)]
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ targetLabel (toCardId card) [search iid attrs iid [fromDeck] (cardIs card) (DrawFound iid 1)]
          | card <- cards
          ]
      pure e
    _ -> LucidDreaming2 <$> runMessage msg attrs
