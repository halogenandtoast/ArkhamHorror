module Arkham.Treachery.Cards.ShockingDisplay (shockingDisplay) where

import Arkham.Card
import Arkham.Helpers.Story (readStory)
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ShockingDisplay = ShockingDisplay TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shockingDisplay :: TreacheryCard ShockingDisplay
shockingDisplay = treachery ShockingDisplay Cards.shockingDisplay

instance RunMessage ShockingDisplay where
  runMessage msg t@(ShockingDisplay attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      storyCards <- select $ UnderScenarioReferenceMatch $ CardWithType StoryType
      for_ (nonEmpty storyCards) \xs -> do
        card <- sample xs
        obtainCard card
        readStory iid card (toCardDef card)
      pure t
    _ -> ShockingDisplay <$> liftRunMessage msg attrs
