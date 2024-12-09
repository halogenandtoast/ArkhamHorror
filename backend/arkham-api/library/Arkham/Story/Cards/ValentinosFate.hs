module Arkham.Story.Cards.ValentinosFate (
  ValentinosFate (..),
  valentinosFate,
  valentinosFateEffect,
) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Creation
import Arkham.Helpers.Investigator
import Arkham.Helpers.Log
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Matcher
import Arkham.SlotType
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner

newtype ValentinosFate = ValentinosFate StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

valentinosFate :: StoryCard ValentinosFate
valentinosFate = story ValentinosFate Cards.valentinosFate

instance RunMessage ValentinosFate where
  runMessage msg s@(ValentinosFate attrs) = case msg of
    ResolveStory iid ResolveIt story' | story' == toId attrs -> do
      disappearedIntoTheMist <-
        toCardCode Investigators.valentinoRivas `inRecordSet` DisappearedIntoTheMist
      claimedBySpecters <-
        toCardCode Investigators.valentinoRivas `inRecordSet` WasClaimedBySpecters
      onTrail <- getHasRecord TheInvestigatorsAreOnValentino'sTrail

      location <- getJustLocation iid

      if disappearedIntoTheMist && onTrail
        then do
          valentino <- getSetAsideCard Assets.valentinoRivas
          enabled <- createCardEffect Cards.valentinosFate Nothing attrs iid
          pushAll
            [ RemoveStory (toId attrs)
            , enabled
            , TakeControlOfSetAsideAsset iid valentino
            , Record ValentinoIsAlive
            ]
        else when claimedBySpecters $ do
          valentino <- genCard Enemies.valentinoRivas
          createValentino <-
            createEnemy valentino
              $ if onTrail
                then toEnemyCreationMethod location
                else toEnemyCreationMethod iid
          pushAll
            [ RemoveStory (toId attrs)
            , toMessage
                $ createValentino
                  { enemyCreationExhausted = onTrail
                  }
            ]

      pure s
    _ -> ValentinosFate <$> runMessage msg attrs

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
