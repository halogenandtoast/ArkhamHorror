module Arkham.Story.Cards.DefuseTheExplosives (defuseTheExplosives) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype DefuseTheExplosives = DefuseTheExplosives StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

defuseTheExplosives :: StoryCard DefuseTheExplosives
defuseTheExplosives = story DefuseTheExplosives Cards.defuseTheExplosives

instance RunMessage DefuseTheExplosives where
  runMessage msg (DefuseTheExplosives attrs) = DefuseTheExplosives <$> runMessage msg attrs
