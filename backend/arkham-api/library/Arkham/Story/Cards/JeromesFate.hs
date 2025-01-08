module Arkham.Story.Cards.JeromesFate (jeromesFate, jeromesFateEffect) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheCircleUndone.Key
import Arkham.Card
import Arkham.Effect.Import
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Creation
import Arkham.Helpers.Investigator
import Arkham.Helpers.Log
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.SlotType
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype JeromesFate = JeromesFate StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jeromesFate :: StoryCard JeromesFate
jeromesFate = story JeromesFate Cards.jeromesFate

instance RunMessage JeromesFate where
  runMessage msg s@(JeromesFate attrs) = runQueueT $ case msg of
    ResolveStory iid ResolveIt story' | story' == toId attrs -> do
      disappearedIntoTheMist <- Investigators.jeromeDavids.cardCode `inRecordSet` DisappearedIntoTheMist
      claimedBySpecters <- Investigators.jeromeDavids.cardCode `inRecordSet` WasClaimedBySpecters
      onTrail <- getHasRecord TheInvestigatorsAreOnJerome'sTrail

      location <- getJustLocation iid

      push $ RemoveStory attrs.id
      if disappearedIntoTheMist && onTrail
        then do
          createCardEffect Cards.jeromesFate Nothing attrs iid
          takeControlOfSetAsideAsset iid =<< getSetAsideCard Assets.jeromeDavids
          record JeromeIsAlive
        else when claimedBySpecters $ do
          jerome <- genCard Enemies.jeromeDavids
          createEnemyWith_
            jerome
            (if onTrail then toEnemyCreationMethod location else toEnemyCreationMethod iid)
            (setExhausted onTrail)
      pure s
    _ -> JeromesFate <$> liftRunMessage msg attrs

newtype JeromesFateEffect = JeromesFateEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jeromesFateEffect :: EffectArgs -> JeromesFateEffect
jeromesFateEffect = cardEffect JeromesFateEffect Cards.jeromesFate

instance HasModifiersFor JeromesFateEffect where
  getModifiersFor (JeromesFateEffect a) =
    modifySelect a (assetIs Assets.jeromeDavids) [DoNotTakeUpSlot AllySlot]

instance RunMessage JeromesFateEffect where
  runMessage _ = pure
