module Arkham.Story.Cards.Demolition (demolition) where

import Arkham.Card
import Arkham.Helpers.Query (getLead)
import Arkham.Location.Types (Field (LocationTokens))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenarios.TheDoomOfArkhamPartII.Helpers
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted
import Arkham.Token (Token (Damage), countTokens)
import Arkham.Trait (Trait (Rooftop))

newtype Demolition = Demolition StoryAttrs
  deriving anyclass (IsStory, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

demolition :: StoryCard Demolition
demolition = story Demolition Cards.demolition

instance RunMessage Demolition where
  runMessage msg s@(Demolition attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      withCthulhuLocation \cthulhuLocation -> do
        rooftops <- select $ NearestLocationToLocation cthulhuLocation (LocationWithTrait Rooftop)
        lead <- getLead
        chooseOrRunOneM lead do
          targets rooftops $ handleTarget lead attrs
      pure s
    HandleTargetChoice _iid (isSource attrs -> True) (LocationTarget lid) -> do
      placeTokens attrs lid Damage 1
      ruins <- fieldMap LocationTokens ((+ 1) . countTokens Damage) lid

      if ruins >= 2
        then do
          selectEach (investigatorAt lid) (kill attrs)
          selectEach (enemyAt lid) (toDiscard attrs)
          removeLocation lid
          retainCthulhuCard (toCard attrs)
        else do
          retainCthulhuCard (toCard attrs)
          scenarioSpecific "reshuffleCthulhuDeck" (toCard attrs)
      pure s
    _ -> Demolition <$> liftRunMessage msg attrs
