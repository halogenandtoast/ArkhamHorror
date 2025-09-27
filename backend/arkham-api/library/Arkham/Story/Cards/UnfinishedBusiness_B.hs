{- HLINT ignore "Use camelCase" -}
module Arkham.Story.Cards.UnfinishedBusiness_B (unfinishedBusiness_B) where

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

newtype UnfinishedBusiness_B = UnfinishedBusiness_B StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unfinishedBusiness_B :: StoryCard UnfinishedBusiness_B
unfinishedBusiness_B = story UnfinishedBusiness_B Cards.unfinishedBusiness_B

instance HasAbilities UnfinishedBusiness_B where
  getAbilities (UnfinishedBusiness_B x) = case x.placement of
    InThreatArea _ ->
      [ restricted x 1 (InThreatAreaOf You) $ forced $ RoundEnds #when
      , skillTestAbility
          $ restricted
            x
            2
            (OnSameLocation <> exists (YourLocation <> "Chapel Crypt" <> LocationWithoutClues))
            actionAbility
      ]
    _ -> []

instance RunMessage UnfinishedBusiness_B where
  runMessage msg s@(UnfinishedBusiness_B attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      pure
        . UnfinishedBusiness_B
        $ attrs
        & (placementL .~ InThreatArea iid)
        & (removeAfterResolutionL .~ False)
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      chooseOneM iid $ withI18n do
        countVar 1 $ labeled' "takeHorror" $ assignHorror iid attrs 1
        labeled' "flipThisBackOver" $ flipOverBy iid (attrs.ability 1) attrs
      pure s
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      chooseBeginSkillTest sid iid (attrs.ability 2) attrs [#intellect, #combat] (Fixed 4)
      pure s
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      let card = lookupCard Enemies.heretic_A (toCardId attrs)
      batched \_ -> do
        checkWhen $ Window.ScenarioEvent "wouldBanish" (Just iid) (toJSON card)
        send $ format card <> " is \"banished\""
        addToVictory attrs
      pure s
    Flip _ _ (isTarget attrs -> True) -> do
      let heretic = lookupCard Enemies.heretic_A (toCardId attrs)
      removeStory attrs
      createEnemy_ heretic attrs.placement
      pure s
    _ -> UnfinishedBusiness_B <$> liftRunMessage msg attrs
