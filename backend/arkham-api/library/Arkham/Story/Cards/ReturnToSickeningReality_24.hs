module Arkham.Story.Cards.ReturnToSickeningReality_24 (returnToSickeningReality_24) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Story
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted.Choose
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted
import Arkham.Target

newtype ReturnToSickeningReality_24 = ReturnToSickeningReality_24 StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToSickeningReality_24 :: StoryCard ReturnToSickeningReality_24
returnToSickeningReality_24 = story ReturnToSickeningReality_24 Cards.returnToSickeningReality_24

instance RunMessage ReturnToSickeningReality_24 where
  runMessage msg s@(ReturnToSickeningReality_24 attrs) = runQueueT $ case msg of
    ResolveStory iid ResolveIt story' | story' == toId attrs -> do
      partyGuests <- select (assetIs Assets.partyGuest)
      if null partyGuests
        then do
          storyCards <-
            select $ UnderScenarioReferenceMatch $ #story <> not_ (cardIs Cards.returnToSickeningReality_24)
          push $ Msg.PlaceUnderneath ScenarioTarget [toCard attrs]
          for_ (nonEmpty storyCards) \xs -> do
            card <- sample xs
            obtainCard card
            readStory iid card (toCardDef card)
        else chooseTargetM iid partyGuests \guest ->
          withLocationOf guest \lid -> do
            selectEach (investigatorAt lid) \iid' -> assignHorror iid' attrs 1
            placeDoomOnAgenda 1
            removeFromGame guest
            createEnemyAt_ Enemies.crazedGuest lid
      pure s
    _ -> ReturnToSickeningReality_24 <$> liftRunMessage msg attrs
