module Arkham.Story.Cards.TracesToNowhere.YoureComingWithMe (youreComingWithMe) where

import Arkham.Scenarios.TracesToNowhere.Helpers
import Arkham.Story.CardDefs.TracesToNowhere qualified as Cards
import Arkham.Story.Import.Lifted

newtype YoureComingWithMe = YoureComingWithMe StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

youreComingWithMe :: StoryCard YoureComingWithMe
youreComingWithMe = story YoureComingWithMe Cards.youreComingWithMe

instance RunMessage YoureComingWithMe where
  runMessage msg s@(YoureComingWithMe attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      culpritParleyBegin attrs #combat
      pure s
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      culpritParleySuccess attrs iid
      pure s
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      culpritParleyFailure attrs iid
      pure s
    _ -> YoureComingWithMe <$> liftRunMessage msg attrs
