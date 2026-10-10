module Arkham.Homebrew.AgesUnwound.Stories.Answers (answers) where

import Arkham.Helpers.Investigator (getHandCount, getHandSize)
import Arkham.Homebrew.AgesUnwound.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Treacheries
import Arkham.Homebrew.AgesUnwound.Helpers (campaignI18n)
import Arkham.Homebrew.AgesUnwound.Missions.Helpers (completeTask)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move (moveTo)
import Arkham.Story.Import.Lifted
import Arkham.Token (Token (Ammo, Charge, Secret, Supply))

newtype Answers = Answers StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | The reverse of /Ancient Sphinx/ (@:ages-unwound:145@).
answers :: StoryCard Answers
answers = story Answers Cards.answers

{- | "You must decide (choose one):
- /"Knowledge."/ Draw cards until you reach your maximum hand size.
- /"Power."/ Distribute 5 supplies, ammo, charges or secrets, in any
  combination, among assets you control.
- /"Wealth."/ Gain 10 resources.
- /"Aid."/ Move to any location. Deal 3 damage to an enemy at that location.

In your Campaign Log, record /you solved the riddle of the sphinx./ Complete
Keeper of Knowledge. Flip this card back over and add it to the victory
display."

"Distribute 5 ... in any combination" is the one place in this set that needs
the @doStep@ countdown: each point is its own choice of asset /and/ of use type,
so each iteration has its own resolution.
-}
instance RunMessage Answers where
  runMessage msg s@(Answers attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      resources <- getHandSize iid
      inHand <- getHandCount iid
      assets <- select $ assetControlledBy iid
      locations <- select Anywhere
      chooseOneM iid $ campaignI18n do
        labeled "answers.knowledge" $ drawCardsIfCan iid attrs (resources - inHand)
        labeledValidate (notNull assets) "answers.power" $ doStep 5 msg
        labeled "answers.wealth" $ gainResources iid attrs 10
        labeled "answers.aid" $ chooseTargetM iid locations \lid -> do
          moveTo attrs iid lid
          enemies <- select $ enemyAt lid
          chooseTargetM iid enemies $ nonAttackEnemyDamage (Just iid) attrs 3
      -- TODO(ages-unwound): no campaign-log key exists for "you solved the
      -- riddle of the sphinx" -- Key.hs carries only the Myriad's version of
      -- the record ('TheMyriadSolvedTheRiddleOfTheSphinx'), and Key.hs belongs
      -- to the orchestrator.
      completeTask Treacheries.keeperOfKnowledge
      for_ (storyOtherSide attrs) (addToVictory iid)
      pure s
    DoStep n (ResolveThisStory iid (is attrs -> True)) | n > 0 -> do
      assets <- select $ assetControlledBy iid
      chooseTargetM iid assets \aid ->
        chooseOneM iid $ campaignI18n do
          labeled "answers.supply" $ placeTokens attrs aid Supply 1
          labeled "answers.ammo" $ placeTokens attrs aid Ammo 1
          labeled "answers.charge" $ placeTokens attrs aid Charge 1
          labeled "answers.secret" $ placeTokens attrs aid Secret 1
      -- `msg`, not the inner ResolveThisStory: doNextStep matches `DoStep n inner`,
      -- so handing it the unwrapped message silently ends the loop after one pass.
      doNextStep msg
      pure s
    _ -> Answers <$> liftRunMessage msg attrs
