module Arkham.Helpers.Story where

import Arkham.Asset.Types (AssetAttrs)
import Arkham.Card
import Arkham.Enemy.Types (EnemyAttrs)
import Arkham.Helpers.Message
import Arkham.Helpers.Window
import Arkham.Id
import Arkham.Location.Types (LocationAttrs)
import Arkham.Message.Lifted
import Arkham.Prelude
import Arkham.Window qualified as Window

class Flippable a where
  flipThis :: (ReverseQueue m, ToId investigator InvestigatorId) => investigator -> a -> m ()

instance Flippable LocationAttrs where
  flipThis investigator attrs = flipThis investigator attrs.id

instance Flippable LocationId where
  flipThis investigator lid = do
    let (whenWindowMsg, _, afterWindowMsg) = frame (Window.FlipLocation (asId investigator) lid)
    pushAll [whenWindowMsg, afterWindowMsg]

instance Flippable EnemyId where
  flipThis _ _ = pure ()

instance Flippable EnemyAttrs where
  flipThis _ _ = pure ()

instance Flippable Card where
  flipThis _ _ = pure ()

instance Flippable AssetAttrs where
  flipThis _ _ = pure ()

readStory
  :: (ReverseQueue m, Flippable a, ToId investigator InvestigatorId)
  => investigator
  -> a
  -> CardDef
  -> m ()
readStory investigator a storyDef = do
  flipThis (asId investigator) a
  storyCard <- genCard storyDef
  push $ ReadStory (asId investigator) storyCard ResolveIt Nothing
