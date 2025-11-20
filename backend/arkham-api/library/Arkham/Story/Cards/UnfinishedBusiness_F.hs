{- HLINT ignore "Use camelCase" -}
module Arkham.Story.Cards.UnfinishedBusiness_F (unfinishedBusiness_F) where

import Arkham.Ability
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.I18n
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Projection
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted
import Arkham.Window qualified as Window

newtype UnfinishedBusiness_F = UnfinishedBusiness_F StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unfinishedBusiness_F :: StoryCard UnfinishedBusiness_F
unfinishedBusiness_F = story UnfinishedBusiness_F Cards.unfinishedBusiness_F

instance HasAbilities UnfinishedBusiness_F where
  getAbilities (UnfinishedBusiness_F x) = case x.placement of
    InThreatArea _ ->
      [ restricted x 1 (InThreatAreaOf You) $ forced $ RoundEnds #when
      , skillTestAbility
          $ restricted
            x
            2
            (OnSameLocation <> exists (YourLocation <> "Chapel Attic" <> LocationWithoutClues))
            actionAbility
      ]
    _ -> []

instance RunMessage UnfinishedBusiness_F where
  runMessage msg s@(UnfinishedBusiness_F attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      pure
        . UnfinishedBusiness_F
        $ attrs
        & (placementL .~ InThreatArea iid)
        & (removeAfterResolutionL .~ False)
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      hasEnoughResources <- fieldMap InvestigatorResources (>= 2) iid
      chooseOneM iid $ withI18n do
        when hasEnoughResources do
          countVar 2 $ labeled' "loseResources" $ loseResources iid attrs 2
        labeled' "flipThisBackOver" $ flipOverBy iid (attrs.ability 1) attrs
      pure s
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      chooseBeginSkillTest sid iid (attrs.ability 2) attrs [#intellect, #agility] (Fixed 4)
      pure s
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      let card = lookupCard Enemies.heretic_E (toCardId attrs)
      batched \_ -> do
        checkWhen $ Window.ScenarioEvent "wouldBanish" (Just iid) (toJSON card)
        send $ format card <> " is \"banished\""
        addToVictory iid attrs
      pure s
    Flip _ _ (isTarget attrs -> True) -> do
      let heretic = lookupCard Enemies.heretic_E (toCardId attrs)
      removeStory attrs
      createEnemy_ heretic attrs.placement
      pure s
    _ -> UnfinishedBusiness_F <$> liftRunMessage msg attrs
