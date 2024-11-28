module Arkham.Story.Cards.CracksInTheIce
  ( CracksInTheIce(..)
  , cracksInTheIce
  ) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype CracksInTheIce = CracksInTheIce StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cracksInTheIce :: StoryCard CracksInTheIce
cracksInTheIce = story CracksInTheIce Cards.cracksInTheIce

instance RunMessage CracksInTheIce where
  runMessage msg s@(CracksInTheIce attrs) = runQueueT $ case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      pure s
    _ -> CracksInTheIce <$> liftRunMessage msg attrs
