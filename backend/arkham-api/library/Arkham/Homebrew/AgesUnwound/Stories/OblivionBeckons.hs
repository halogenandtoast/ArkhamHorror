module Arkham.Homebrew.AgesUnwound.Stories.OblivionBeckons (oblivionBeckons) where

import Arkham.Homebrew.AgesUnwound.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.AgesUnwound.Key
import Arkham.Homebrew.AgesUnwound.Scenarios.TimeRunsOut.Helpers (chooseAndExileFromHand)
import Arkham.Matcher
import Arkham.Story.Import.Lifted

newtype OblivionBeckons = OblivionBeckons StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | The reverse of the @:ages-unwound:203@ printing of /The End of All Things/.
oblivionBeckons :: StoryCard OblivionBeckons
oblivionBeckons = story OblivionBeckons Cards.oblivionBeckons

{- | "Each investigator tests [combat] (4). Each investigator who fails takes 1
damage for each point they failed by. /
Each investigator tests [willpower] (4). Each investigator who fails takes 1
horror for each point they failed by. /
Each investigator whose 'existence is waning' tests [agility] (4). Each
investigator who fails chooses and exiles a card from their hand for each point
they failed by. /
Remove The End of All Things from the game."

Three rounds of tests in printed order, told apart by 'indexed'. Damage and
horror are single assignments of X; the exile is not -- each point is its own
choice of card -- so only the third gets the 'doStep' countdown.
-}
instance RunMessage OblivionBeckons where
  runMessage msg s@(OblivionBeckons attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      eachInvestigator \iid -> do
        sid <- getRandom
        beginSkillTest sid iid (indexed 1 attrs) iid #combat (Fixed 4)
      eachInvestigator \iid -> do
        sid <- getRandom
        beginSkillTest sid iid (indexed 2 attrs) iid #willpower (Fixed 4)
      selectEach (investigatorWithRecord ExistenceIsWaning) \iid -> do
        sid <- getRandom
        beginSkillTest sid iid (indexed 3 attrs) iid #agility (Fixed 4)
      for_ (storyOtherSide attrs) removeFromGame
      removeFromGame attrs
      pure s
    FailedThisSkillTestBy iid source n | isIndexedSource 1 attrs source -> do
      assignDamage iid attrs n
      pure s
    FailedThisSkillTestBy iid source n | isIndexedSource 2 attrs source -> do
      assignHorror iid attrs n
      pure s
    FailedThisSkillTestBy _ source n | isIndexedSource 3 attrs source -> do
      doStep n msg
      pure s
    DoStep n (FailedThisSkillTestBy iid source _)
      | n > 0
      , isIndexedSource 3 attrs source -> do
          chooseAndExileFromHand iid
          doNextStep msg
          pure s
    _ -> OblivionBeckons <$> liftRunMessage msg attrs
