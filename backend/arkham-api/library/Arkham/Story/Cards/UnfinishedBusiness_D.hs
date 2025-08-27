{- HLINT ignore "Use camelCase" -}
module Arkham.Story.Cards.UnfinishedBusiness_D (unfinishedBusiness_D) where

import Arkham.Ability
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

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
      chooseOneM iid do
        labeled "Take 1 damage" $ assignDamage iid (attrs.ability 1) 1
        labeled "Flip this back over" $ flipOverBy iid (attrs.ability 1) attrs
      pure s
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      chooseBeginSkillTest sid iid (attrs.ability 2) attrs [#willpower, #agility] (Fixed 4)
      pure s
    PassedSkillTest _ _ (isAbilitySource attrs 2 -> True) SkillTestInitiatorTarget {} _ _ -> do
      card <- genCard Enemies.heretic_C
      send $ format card <> " is \"banished\""
      push $ AddToVictory (toTarget attrs)
      pure s
    Flip _ _ (isTarget attrs -> True) -> do
      let heretic = lookupCard Enemies.heretic_C (toCardId attrs)
      removeStory attrs
      createEnemy_ heretic attrs.placement
      pure s
    _ -> UnfinishedBusiness_D <$> liftRunMessage msg attrs
