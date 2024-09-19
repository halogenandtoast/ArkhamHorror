module Arkham.Story.Cards.FindingAgentHarper (FindingAgentHarper (..), findingAgentHarper) where

import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Matcher
import Arkham.Name
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted
import Arkham.Target

newtype Meta = Meta {crossedOff :: [Text]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype FindingAgentHarper = FindingAgentHarper StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

findingAgentHarper :: StoryCard FindingAgentHarper
findingAgentHarper = story FindingAgentHarper Cards.findingAgentHarper

instance RunMessage FindingAgentHarper where
  runMessage msg s@(FindingAgentHarper attrs) = runQueueT $ case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      pure s
    ForTarget (isTarget attrs -> True) (RevealCard cid) -> do
      card <- getCard cid
      let meta = toResultDefault (Meta []) attrs.meta
      pure
        . FindingAgentHarper
        $ attrs
        & metaL
        .~ toJSON (meta {crossedOff = toTitle card : crossedOff meta})
    Flip _ _ (isTarget attrs -> True) -> do
      let angryMob = lookupCard Enemies.angryMob (toCardId attrs)
      push $ RemoveStory (toId attrs)
      innsmouthSquare <- selectJust $ location_ "Innsmouth Square"
      createEnemyAt_ angryMob innsmouthSquare
      pure s
    _ -> FindingAgentHarper <$> liftRunMessage msg attrs
