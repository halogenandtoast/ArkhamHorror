module Arkham.Story.Cards.GavriellasFate (gavriellasFate, gavriellasFateEffect) where

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

newtype GavriellasFate = GavriellasFate StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gavriellasFate :: StoryCard GavriellasFate
gavriellasFate = story GavriellasFate Cards.gavriellasFate

instance RunMessage GavriellasFate where
  runMessage msg s@(GavriellasFate attrs) = runQueueT $ case msg of
    ResolveStory iid ResolveIt story' | story' == toId attrs -> do
      disappearedIntoTheMist <-
        Investigators.gavriellaMizrah.cardCode `inRecordSet` DisappearedIntoTheMist
      claimedBySpecters <- Investigators.gavriellaMizrah.cardCode `inRecordSet` WasClaimedBySpecters
      onTrail <- getHasRecord TheInvestigatorsAreOnGavriella'sTrail

      if disappearedIntoTheMist && onTrail
        then do
          push $ RemoveStory (toId attrs)
          createCardEffect Cards.gavriellasFate Nothing attrs iid
          takeControlOfSetAsideAsset iid =<< getSetAsideCard Assets.gavriellaMizrah
          record GavriellaIsAlive
        else when claimedBySpecters do
          gavriella <- genCard Enemies.gavriellaMizrah
          location <- getJustLocation iid
          push $ RemoveStory attrs.id
          createEnemyWith_
            gavriella
            (if onTrail then toEnemyCreationMethod location else toEnemyCreationMethod iid)
            (setExhausted onTrail)

      pure s
    _ -> GavriellasFate <$> liftRunMessage msg attrs

newtype GavriellasFateEffect = GavriellasFateEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gavriellasFateEffect :: EffectArgs -> GavriellasFateEffect
gavriellasFateEffect = cardEffect GavriellasFateEffect Cards.gavriellasFate

instance HasModifiersFor GavriellasFateEffect where
  getModifiersFor (GavriellasFateEffect a) =
    modifySelect a (assetIs Assets.gavriellaMizrah) [DoNotTakeUpSlot AllySlot]

instance RunMessage GavriellasFateEffect where
  runMessage _ = pure
