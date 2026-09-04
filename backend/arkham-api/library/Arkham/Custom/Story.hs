-- | The runner behind a debug-authored custom story. See "Arkham.Custom.Enemy".
module Arkham.Custom.Story (CustomStory (..), customStory) where

import Arkham.Card.CardDef (CardDef)
import Arkham.Custom.Ability (customAbilities, customModifiers, runCustomAbility, runCustomHandlers)
import Arkham.Story.Import.Lifted

newtype CustomStory = CustomStory StoryAttrs
  deriving anyclass IsStory
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

customStory :: CardDef -> StoryCard CustomStory
customStory = story CustomStory

instance HasModifiersFor CustomStory where
  getModifiersFor (CustomStory a) = customModifiers a

instance HasAbilities CustomStory where
  getAbilities (CustomStory a) = customAbilities a

instance RunMessage CustomStory where
  runMessage msg x@(CustomStory attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) idx -> do
      runCustomAbility attrs iid idx
      pure x
    _ -> do
      runCustomHandlers attrs msg
      CustomStory <$> liftRunMessage msg attrs
