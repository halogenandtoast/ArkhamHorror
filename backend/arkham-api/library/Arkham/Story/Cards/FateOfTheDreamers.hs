module Arkham.Story.Cards.FateOfTheDreamers (fateOfTheDreamers) where

import Arkham.Campaigns.GuardiansOfTheAbyss.Helpers
import Arkham.Message.Lifted.Choose
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype FateOfTheDreamers = FateOfTheDreamers StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fateOfTheDreamers :: StoryCard FateOfTheDreamers
fateOfTheDreamers = story FateOfTheDreamers Cards.fateOfTheDreamers

instance RunMessage FateOfTheDreamers where
  runMessage msg s@(FateOfTheDreamers attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      campaignI18n $ chooseOneM iid do
        labeled' "fateOfTheDreamers.removeStrength" $ removeStrengthOfTheAbyss 2
        labeled' "fateOfTheDreamers.addStrength" $ addStrengthOfTheAbyss 2
      pure s
    _ -> FateOfTheDreamers <$> liftRunMessage msg attrs
