module Arkham.Homebrew.AgesUnwound.Stories.Erasure (erasure) where

import Arkham.Homebrew.AgesUnwound.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.AgesUnwound.Key
import Arkham.Homebrew.AgesUnwound.Scenarios.TimeRunsOut.Helpers (timeRunsOutI18n)
import Arkham.I18n
import Arkham.Id
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.SkillType
import Arkham.Story.Import.Lifted

newtype Erasure = Erasure StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | The reverse of the @:ages-unwound:199@ printing of /What Could Never Be/.
erasure :: StoryCard Erasure
erasure = story Erasure Cards.erasure

{- | The four tests, in printed order, with the 'Arkham.Source.IndexedSource'
index each one carries so its result can be told from the others'.
-}
erasureTests :: [(Int, SkillType, Text)]
erasureTests =
  [ (1, SkillWillpower, "testWillpower")
  , (2, SkillIntellect, "testIntellect")
  , (3, SkillCombat, "testCombat")
  , (4, SkillAgility, "testAgility")
  ]

-- | One entry per test failed, so an investigator's tally is how often they appear.
failures :: StoryAttrs -> [InvestigatorId]
failures a = toResultDefault [] a.meta

{- | "Each investigator tests [willpower] (3), [intellect] (3), [combat] (3) and
[agility] (3), in any order. If 'your existence is waning', each of these tests
gets +1 difficulty for you. Each investigator takes 1 damage and 1 horror for each
of these tests that they fail. Each investigator that fails at least three of
these tests is killed. Remove What Could Never Be from the game."

"In any order" is a real choice -- it is how an investigator sequences around
being defeated -- so the four tests are offered with 'chooseOneAtATimeM' rather
than fixed.

The kill is checked against a running tally in the story's meta, because "fails at
least three of these tests" spans four separate tests and nothing on the
investigator records which card failed them. The tally is a list rather than a map
so it needs no @FromJSONKey@.
-}
instance RunMessage Erasure where
  runMessage msg s@(Erasure attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      eachInvestigator \iid -> do
        waning <- iid <=~> investigatorWithRecord ExistenceIsWaning
        let difficulty = if waning then 4 else 3
        chooseOneAtATimeM iid $ timeRunsOutI18n $ scope "erasure" do
          for_ erasureTests \(i, sk, lbl) -> labeled lbl do
            sid <- getRandom
            beginSkillTest sid iid (indexed i attrs) iid sk (Fixed difficulty)
      for_ (storyOtherSide attrs) removeFromGame
      removeFromGame attrs
      pure s
    FailedThisSkillTest iid source
      | any (\(i, _, _) -> isIndexedSource i attrs source) erasureTests -> do
          assignDamage iid attrs 1
          assignHorror iid attrs 1
          let tally = iid : failures attrs
          when (count (== iid) tally == 3) $ kill attrs iid
          pure $ Erasure $ attrs & metaL .~ toJSON tally
    _ -> Erasure <$> liftRunMessage msg attrs
