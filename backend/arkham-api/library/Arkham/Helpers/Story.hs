module Arkham.Helpers.Story where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes.HasQueue
import Arkham.Enemy.Types (EnemyAttrs)
import Arkham.Helpers.Window
import Arkham.Id
import Arkham.Location.Types (LocationAttrs)
import Arkham.Message
import Arkham.Message.Lifted
import Arkham.Window qualified as Window

class Flippable a where
  flipThis
    :: (ReverseQueue m, AsId investigator, IdOf investigator ~ InvestigatorId) => investigator -> a -> m ()

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

readStory
  :: (ReverseQueue m, Flippable a, AsId investigator, IdOf investigator ~ InvestigatorId)
  => investigator
  -> a
  -> CardDef
  -> m ()
readStory investigator a storyDef = do
  flipThis (asId investigator) a
  storyCard <- genCard storyDef
  push $ ReadStory (asId investigator) storyCard ResolveIt Nothing
