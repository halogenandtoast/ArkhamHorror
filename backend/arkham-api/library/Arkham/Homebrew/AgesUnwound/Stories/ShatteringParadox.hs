module Arkham.Homebrew.AgesUnwound.Stories.ShatteringParadox (shatteringParadox) where

import Arkham.Helpers.SkillTest.Lifted (combinationSkillTest)
import Arkham.Homebrew.AgesUnwound.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.TimeRunsOut.Helpers
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.SkillType
import Arkham.Source
import Arkham.Story.Import.Lifted

newtype ShatteringParadox = ShatteringParadox StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | The reverse of the @:ages-unwound:200@ printing of /What Could Never Be/.
shatteringParadox :: StoryCard ShatteringParadox
shatteringParadox = story ShatteringParadox Cards.shatteringParadox

{- | Which of the three bullets have already been taken, so "a different option"
can be enforced across the three tests. Numbered as they are printed.
-}
chosenOptions :: StoryAttrs -> [Int]
chosenOptions a = toResultDefault [] a.meta

-- | Whether a source is one of this card's three tests.
isParadoxTest :: StoryAttrs -> Source -> Bool
isParadoxTest a source = any (\i -> isIndexedSource i a source) [1 :: Int, 2, 3]

{- | "One investigator tests [willpower] (5). One investigator tests [intellect]
(5). One investigator tests [willpower]+[intellect] (8). For each of these tests
that is failed, the performing investigator must choose a different option:
-- Place 1 doom on the current agenda. This can cause the current agenda to
advance.
-- Choose a random warded location in play, and place it beneath the agenda deck.
-- The performing investigator takes 6 horror.
Remove What Could Never Be from the game."

"One investigator" is the table's call, so each test is a lead prompt over
everyone. "A different option" is tracked in the story's meta: there are three
options and at most three failures, so by the third failure the choice is forced,
which is why the prompt is 'chooseOrRunOneM'. The branches route through 'doStep'
rather than running inline because the chosen branch has to write itself into that
meta, and only a message handler can.
-}
instance RunMessage ShatteringParadox where
  runMessage msg s@(ShatteringParadox attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      investigators <- select UneliminatedInvestigator
      for_ ([(1, SkillWillpower), (2, SkillIntellect)] :: [(Int, SkillType)]) \(i, sk) ->
        leadChooseOrRunOneM $ targets investigators \iid -> do
          sid <- getRandom
          beginSkillTest sid iid (indexed i attrs) iid sk (Fixed 5)
      leadChooseOrRunOneM $ targets investigators \iid -> do
        sid <- getRandom
        combinationSkillTest sid iid (indexed 3 attrs) iid [SkillWillpower, SkillIntellect] (Fixed 8)
      for_ (storyOtherSide attrs) removeFromGame
      removeFromGame attrs
      pure s
    FailedThisSkillTest iid (isParadoxTest attrs -> True) -> do
      let taken = chosenOptions attrs
      wardedLocation <- getRandomLocation warded
      chooseOrRunOneM iid $ timeRunsOutI18n $ scope "shatteringParadox" do
        labeledValidate (1 `notElem` taken) "placeDoom" $ doStep 1 msg
        labeledValidate (2 `notElem` taken && isJust wardedLocation) "shelveWardedLocation"
          $ doStep 2 msg
        labeledValidate (3 `notElem` taken) "takeHorror" $ doStep 3 msg
      pure s
    DoStep 1 (FailedThisSkillTest _ (isParadoxTest attrs -> True)) -> do
      placeDoomOnAgendaAndCheckAdvance 1
      pure $ ShatteringParadox $ attrs & metaL .~ toJSON (1 : chosenOptions attrs)
    DoStep 2 (FailedThisSkillTest _ (isParadoxTest attrs -> True)) -> do
      getRandomLocation warded >>= traverse_ placeBeneathAgendaDeck
      pure $ ShatteringParadox $ attrs & metaL .~ toJSON (2 : chosenOptions attrs)
    DoStep 3 (FailedThisSkillTest iid (isParadoxTest attrs -> True)) -> do
      assignHorror iid attrs 6
      pure $ ShatteringParadox $ attrs & metaL .~ toJSON (3 : chosenOptions attrs)
    _ -> ShatteringParadox <$> liftRunMessage msg attrs
