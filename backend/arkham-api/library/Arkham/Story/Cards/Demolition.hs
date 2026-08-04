module Arkham.Story.Cards.Demolition (demolition) where

import Arkham.Matcher
import Arkham.Projection
import Arkham.Scenarios.TheDoomOfArkhamPartII.Helpers
import Arkham.Card
import Arkham.Location.Types (Field (LocationTokens))
import Arkham.Trait (Trait (Rooftop))
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted
import Arkham.Token (Token (Damage), countTokens)

newtype Demolition = Demolition StoryAttrs
  deriving anyclass (IsStory, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

demolition :: StoryCard Demolition
demolition = story Demolition Cards.demolition

instance RunMessage Demolition where
  runMessage msg s@(Demolition attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      getCthulhuLocation >>= traverse_ \cthulhuLocation -> do
        -- "Place 1 damage token on the [[Rooftop]] location nearest to Cthulhu, as a
        -- ruin token."
        rooftop <- selectOne $ NearestLocationToLocation cthulhuLocation (LocationWithTrait Rooftop)
        for_ rooftop \lid -> do
          placeTokens attrs lid Damage 1
          ruins <- fieldMap LocationTokens (countTokens Damage) lid

          {- "If there are 2 or more ruin tokens on that location, each investigator is
          killed and each enemy at that location is discarded. Then, remove that
          location and this card from the game." The kill is written without a
          location qualifier, in deliberate contrast to the enemies immediately after
          it, so it takes the whole party. -}
          if ruins >= 2
            then do
              selectEach UneliminatedInvestigator (kill attrs)
              selectEach (enemyAt lid) (toDiscard attrs)
              push $ RemoveLocation lid
              {- Removed from the game: dropping out of the discard is enough, since
              the card only ever re-enters play by being reshuffled from there. -}
              retainCthulhuCard (toCard attrs)
            else
              -- "Otherwise, shuffle this card into the Cthulhu deck along with the
              -- Cthulhu discard pile."
              retainCthulhuCard (toCard attrs)
                *> push (ScenarioSpecific "reshuffleCthulhuDeck" (toJSON $ toCard attrs))
      pure s
    _ -> Demolition <$> liftRunMessage msg attrs
