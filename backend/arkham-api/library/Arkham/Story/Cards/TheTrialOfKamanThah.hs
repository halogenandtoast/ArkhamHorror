module Arkham.Story.Cards.TheTrialOfKamanThah (theTrialOfKamanThah) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype TheTrialOfKamanThah = TheTrialOfKamanThah StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTrialOfKamanThah :: StoryCard TheTrialOfKamanThah
theTrialOfKamanThah = story TheTrialOfKamanThah Cards.theTrialOfKamanThah

instance RunMessage TheTrialOfKamanThah where
  runMessage msg s@(TheTrialOfKamanThah attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      addToVictory attrs
      pure s
    _ -> TheTrialOfKamanThah <$> liftRunMessage msg attrs
