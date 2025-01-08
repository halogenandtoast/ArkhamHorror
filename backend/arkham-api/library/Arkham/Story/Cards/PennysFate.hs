module Arkham.Story.Cards.PennysFate (pennysFate, pennysFateEffect) where

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

newtype PennysFate = PennysFate StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pennysFate :: StoryCard PennysFate
pennysFate = story PennysFate Cards.pennysFate

instance RunMessage PennysFate where
  runMessage msg s@(PennysFate attrs) = runQueueT $ case msg of
    ResolveStory iid ResolveIt story' | story' == toId attrs -> do
      disappearedIntoTheMist <- toCardCode Investigators.pennyWhite `inRecordSet` DisappearedIntoTheMist
      claimedBySpecters <- toCardCode Investigators.pennyWhite `inRecordSet` WasClaimedBySpecters
      onTrail <- getHasRecord TheInvestigatorsAreOnPenny'sTrail

      push $ RemoveStory attrs.id
      if disappearedIntoTheMist && onTrail
        then do
          createCardEffect Cards.pennysFate Nothing attrs iid
          takeControlOfSetAsideAsset iid =<< getSetAsideCard Assets.pennyWhite
          record PennyIsAlive
        else when claimedBySpecters $ do
          location <- getJustLocation iid
          penny <- genCard Enemies.pennyWhite
          createEnemyWith_
            penny
            (if onTrail then toEnemyCreationMethod location else toEnemyCreationMethod iid)
            (setExhausted onTrail)
      pure s
    _ -> PennysFate <$> liftRunMessage msg attrs

newtype PennysFateEffect = PennysFateEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pennysFateEffect :: EffectArgs -> PennysFateEffect
pennysFateEffect = cardEffect PennysFateEffect Cards.pennysFate

instance HasModifiersFor PennysFateEffect where
  getModifiersFor (PennysFateEffect a) =
    modifySelect a (assetIs Assets.pennyWhite) [DoNotTakeUpSlot AllySlot]

instance RunMessage PennysFateEffect where
  runMessage _ = pure
