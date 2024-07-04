module Arkham.Event.Cards.WordOfWeal (wordOfWeal, WordOfWeal (..)) where

import Arkham.Deck
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier

newtype WordOfWeal = WordOfWeal EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wordOfWeal :: EventCard WordOfWeal
wordOfWeal = event WordOfWeal Cards.wordOfWeal

instance RunMessage WordOfWeal where
  runMessage msg e@(WordOfWeal attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      skillTestModifier attrs iid (AddSkillValue #willpower)

      wordOfWoe <- selectOne $ inDiscardOf iid <> basic (cardIs Cards.wordOfWoe)

      for_ wordOfWoe \card -> do
        chooseOne
          iid
          [ Label "Shuffle Word of Woe into your deck" [ShuffleCardsIntoDeck (toDeck iid) [card]]
          , Label "Do not shuffle it back in" []
          ]
      pure e
    _ -> WordOfWeal <$> liftRunMessage msg attrs
