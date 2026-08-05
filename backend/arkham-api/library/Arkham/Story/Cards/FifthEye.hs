module Arkham.Story.Cards.FifthEye (fifthEye) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Placement
import Arkham.Scenarios.TheDoomOfArkhamPartII.Helpers
import Arkham.Helpers.Query (getLead)
import Arkham.Message.Lifted.Choose
import Arkham.Card
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype FifthEye = FifthEye StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fifthEye :: StoryCard FifthEye
fifthEye = persistStory $ story FifthEye Cards.fifthEye

instance HasAbilities FifthEye where
  getAbilities (FifthEye a) =
    -- "[Forced] - When the investigation phase ends: Place 1 doom on an enemy on
    -- the Cthulhu Board with no doom on it and discard this card."
    [mkAbility a 1 $ forced $ PhaseEnds #when #investigation | a.placement == NextToAgenda]

instance RunMessage FifthEye where
  runMessage msg s@(FifthEye attrs) = runQueueT $ case msg of
    -- "Put this card into play next to the agenda deck."
    ResolveThisStory _ (is attrs -> True) -> do
      retainCthulhuCard (toCard attrs)
      pure $ FifthEye $ attrs & placementL .~ NextToAgenda
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      candidates <- select $ mapOneOf (cthulhuFacet . snd) cthulhuBoardSlots <> EnemyWithDoom (static 0)
      lead <- getLead
      chooseOrRunOneM lead $ scenarioI18n do
        questionLabeled' "chooseEnemyForDoom"
        targets candidates \eid -> placeDoom (attrs.ability 1) eid 1
      discardCthulhuCard attrs
      pure s
    _ -> FifthEye <$> liftRunMessage msg attrs
