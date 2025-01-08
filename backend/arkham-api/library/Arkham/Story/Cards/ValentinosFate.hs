module Arkham.Story.Cards.ValentinosFate (valentinosFate, valentinosFateEffect) where

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

newtype ValentinosFate = ValentinosFate StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

valentinosFate :: StoryCard ValentinosFate
valentinosFate = story ValentinosFate Cards.valentinosFate

instance RunMessage ValentinosFate where
  runMessage msg s@(ValentinosFate attrs) = runQueueT $ case msg of
    ResolveStory iid ResolveIt story' | story' == toId attrs -> do
      disappearedIntoTheMist <-
        Investigators.valentinoRivas.cardCode `inRecordSet` DisappearedIntoTheMist
      claimedBySpecters <-
        Investigators.valentinoRivas.cardCode `inRecordSet` WasClaimedBySpecters
      onTrail <- getHasRecord TheInvestigatorsAreOnValentino'sTrail

      location <- getJustLocation iid

      push $ RemoveStory attrs.id
      if disappearedIntoTheMist && onTrail
        then do
          createCardEffect Cards.valentinosFate Nothing attrs iid
          takeControlOfSetAsideAsset iid =<< getSetAsideCard Assets.valentinoRivas
          record ValentinoIsAlive
        else when claimedBySpecters $ do
          valentino <- genCard Enemies.valentinoRivas
          createEnemyWith_
            valentino
            (if onTrail then toEnemyCreationMethod location else toEnemyCreationMethod iid)
            (setExhausted onTrail)
      pure s
    _ -> ValentinosFate <$> liftRunMessage msg attrs

newtype ValentinosFateEffect = ValentinosFateEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

valentinosFateEffect :: EffectArgs -> ValentinosFateEffect
valentinosFateEffect = cardEffect ValentinosFateEffect Cards.valentinosFate

instance HasModifiersFor ValentinosFateEffect where
  getModifiersFor (ValentinosFateEffect a) =
    modifySelect a (assetIs Assets.valentinoRivas) [DoNotTakeUpSlot AllySlot]

instance RunMessage ValentinosFateEffect where
  runMessage _ = pure
