{- HLINT ignore "Use camelCase" -}
module Arkham.Story.Cards.UnfinishedBusiness_D (unfinishedBusiness_D) where

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

newtype UnfinishedBusiness_D = UnfinishedBusiness_D StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unfinishedBusiness_D :: StoryCard UnfinishedBusiness_D
unfinishedBusiness_D = story UnfinishedBusiness_D Cards.unfinishedBusiness_D

instance HasAbilities UnfinishedBusiness_D where
  getAbilities (UnfinishedBusiness_D x) = case x.placement of
    InThreatArea _ ->
      [ restricted x 1 (InThreatAreaOf You) $ forced $ RoundEnds #when
      , skillTestAbility
          $ restricted
            x
            2
            (OnSameLocation <> exists (YourLocation <> "The Gallows" <> LocationWithoutClues))
            actionAbility
      ]
    _ -> []

instance RunMessage UnfinishedBusiness_D where
  runMessage msg s@(UnfinishedBusiness_D attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      pure
        . UnfinishedBusiness_D
        $ attrs
        & (placementL .~ InThreatArea iid)
        & (removeAfterResolutionL .~ False)
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseOneM iid $ withI18n do
        countVar 1 $ labeled' "takeDamage" $ assignDamage iid (attrs.ability 1) 1
        labeled' "flipThisBackOver" $ flipOverBy iid (attrs.ability 1) attrs
      pure s
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      chooseBeginSkillTest sid iid (attrs.ability 2) attrs [#willpower, #agility] (Fixed 4)
      pure s
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      let card = lookupCard Enemies.heretic_C (toCardId attrs)
      batched \_ -> do
        checkWhen $ Window.ScenarioEvent "wouldBanish" (Just iid) (toJSON card)
        send $ format card <> " is \"banished\""
        push $ AddToVictory (toTarget attrs)
      pure s
    Flip _ _ (isTarget attrs -> True) -> do
      let heretic = lookupCard Enemies.heretic_C (toCardId attrs)
      removeStory attrs
      createEnemy_ heretic attrs.placement
      pure s
    _ -> UnfinishedBusiness_D <$> liftRunMessage msg attrs
