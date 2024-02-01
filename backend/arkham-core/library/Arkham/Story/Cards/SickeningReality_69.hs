module Arkham.Story.Cards.SickeningReality_69 (
  SickeningReality_69 (..),
  sickeningReality_69,
) where

import Arkham.Prelude

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner

newtype SickeningReality_69 = SickeningReality_69 StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

sickeningReality_69 :: StoryCard SickeningReality_69
sickeningReality_69 = story SickeningReality_69 Cards.sickeningReality_69

instance RunMessage SickeningReality_69 where
  runMessage msg s@(SickeningReality_69 attrs) = case msg of
    ResolveStory _ _ story' | story' == toId attrs -> do
      pure s
    _ -> SickeningReality_69 <$> runMessage msg attrs
