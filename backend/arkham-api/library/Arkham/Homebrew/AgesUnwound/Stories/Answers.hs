module Arkham.Homebrew.AgesUnwound.Stories.Answers (answers) where

import Arkham.Capability
import Arkham.Helpers.Investigator (getHandCount, getHandSize)
import Arkham.Homebrew.AgesUnwound.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Treacheries
import Arkham.Homebrew.AgesUnwound.Helpers (campaignI18n)
import Arkham.Homebrew.AgesUnwound.Key
import Arkham.Homebrew.AgesUnwound.Missions.Helpers (completeTask)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log (record)
import Arkham.Message.Lifted.Move (moveTo)
import Arkham.Story.Import.Lifted
import Arkham.Token (Token (Ammo, Charge, Secret, Supply))
import Data.List.Extra (nubOrd)

newtype Answers = Answers StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

answers :: StoryCard Answers
answers = story Answers Cards.answers

instance RunMessage Answers where
  runMessage msg s@(Answers attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      resources <- getHandSize iid
      inHand <- getHandCount iid
      assets <- select $ assetControlledBy iid
      locations <- select $ canEnterLocation iid

      resourceOk <- can.gain.resources iid
      drawOk <- (resources - inHand > 0 &&) <$> can.draw.cards iid

      chooseOneM iid $ campaignI18n do
        labeledValidate drawOk "answers.knowledge" $ drawCardsIfCan iid attrs (resources - inHand)
        labeledValidate (notNull assets) "answers.power" $ doStep 5 msg
        labeledValidate resourceOk "answers.wealth" $ gainResources iid attrs 10
        labeledValidate (notNull locations) "answers.aid" $ chooseTargetM iid locations \lid -> do
          moveTo attrs iid lid
          enemies <- select $ enemyAt lid
          chooseTargetM iid enemies $ nonAttackEnemyDamage (Just iid) attrs 3
      record YouSolvedTheRiddleOfTheSphinx
      completeTask Treacheries.keeperOfKnowledge
      removeStory attrs
      for_ (storyOtherSide attrs) (addToVictory iid)
      pure s
    DoStep n (ResolveThisStory iid (is attrs -> True)) | n > 0 -> do
      let getUseAssets use = select $ assetControlledBy iid <> AssetCanHaveUses use
      supplyAssets <- getUseAssets Supply
      ammoAssets <- getUseAssets Ammo
      chargeAssets <- getUseAssets Charge
      secretAssets <- getUseAssets Secret
      let assets = nubOrd $ supplyAssets <> ammoAssets <> chargeAssets <> secretAssets
      chooseTargetM iid assets \aid ->
        chooseOneM iid $ campaignI18n do
          when (aid `elem` supplyAssets) do
            labeled "answers.supply" $ placeTokens attrs aid Supply 1
          when (aid `elem` ammoAssets) do
            labeled "answers.ammo" $ placeTokens attrs aid Ammo 1
          when (aid `elem` chargeAssets) do
            labeled "answers.charge" $ placeTokens attrs aid Charge 1
          when (aid `elem` secretAssets) do
            labeled "answers.secret" $ placeTokens attrs aid Secret 1
      doNextStep msg
      pure s
    _ -> Answers <$> liftRunMessage msg attrs
