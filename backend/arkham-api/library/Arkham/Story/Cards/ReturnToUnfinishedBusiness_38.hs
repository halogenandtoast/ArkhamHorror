module Arkham.Story.Cards.ReturnToUnfinishedBusiness_38 (returnToUnfinishedBusiness_38) where

import Arkham.Ability
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted
import Arkham.Window qualified as Window

newtype ReturnToUnfinishedBusiness_38 = ReturnToUnfinishedBusiness_38 StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToUnfinishedBusiness_38 :: StoryCard ReturnToUnfinishedBusiness_38
returnToUnfinishedBusiness_38 = story ReturnToUnfinishedBusiness_38 Cards.returnToUnfinishedBusiness_38

instance HasAbilities ReturnToUnfinishedBusiness_38 where
  getAbilities (ReturnToUnfinishedBusiness_38 x) = case x.placement of
    InThreatArea _ ->
      [ restricted x 1 (InThreatAreaOf You) $ forced $ RoundEnds #when
      , skillTestAbility
          $ restricted
            x
            2
            (OnSameLocation <> exists (YourLocation <> "Hangman's Brook" <> LocationWithoutClues))
            actionAbility
      ]
    _ -> []

instance RunMessage ReturnToUnfinishedBusiness_38 where
  runMessage msg s@(ReturnToUnfinishedBusiness_38 attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      pure
        . ReturnToUnfinishedBusiness_38
        $ attrs
        & (placementL .~ InThreatArea iid)
        & (removeAfterResolutionL .~ False)
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseOneM iid $ withI18n do
        countVar 1 $ labeled' "discardAssets" $ chooseAndDiscardAsset iid (attrs.ability 1)
        labeled' "flipThisBackOver" $ flipOverBy iid (attrs.ability 1) attrs
      pure s
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      chooseBeginSkillTest sid iid (attrs.ability 2) attrs [minBound ..] (Fixed 5)
      pure s
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      let card = lookupCard Enemies.returnToHeretic_38 (toCardId attrs)
      batched \_ -> do
        checkWhen $ Window.ScenarioEvent "wouldBanish" (Just iid) (toJSON card)
        send $ format card <> " is \"banished\""
        addToVictory iid attrs
      pure s
    Flip _ _ (isTarget attrs -> True) -> do
      let heretic = lookupCard Enemies.returnToHeretic_38 (toCardId attrs)
      removeStory attrs
      createEnemy_ heretic attrs.placement
      pure s
    _ -> ReturnToUnfinishedBusiness_38 <$> liftRunMessage msg attrs
