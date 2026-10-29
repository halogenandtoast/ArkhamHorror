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

erasure :: StoryCard Erasure
erasure = story Erasure Cards.erasure

erasureTests :: [(Int, SkillType, Text)]
erasureTests =
  [ (1, #willpower, "testWillpower")
  , (2, #intellect, "testIntellect")
  , (3, #combat, "testCombat")
  , (4, #agility, "testAgility")
  ]

failures :: StoryAttrs -> [InvestigatorId]
failures a = toResultDefault [] a.meta

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
          assignDamageAndHorror iid attrs 1 1
          let tally = iid : failures attrs
          when (count (== iid) tally == 3) $ kill attrs iid
          pure $ Erasure $ attrs & metaL .~ toJSON tally
    _ -> Erasure <$> liftRunMessage msg attrs
