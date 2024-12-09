module Arkham.Story.Cards.JeromesFate (
  JeromesFate (..),
  jeromesFate,
  jeromesFateEffect,
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

newtype JeromesFate = JeromesFate StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jeromesFate :: StoryCard JeromesFate
jeromesFate = story JeromesFate Cards.jeromesFate

instance RunMessage JeromesFate where
  runMessage msg s@(JeromesFate attrs) = case msg of
    ResolveStory iid ResolveIt story' | story' == toId attrs -> do
      disappearedIntoTheMist <-
        toCardCode Investigators.jeromeDavids `inRecordSet` DisappearedIntoTheMist
      claimedBySpecters <-
        toCardCode Investigators.jeromeDavids `inRecordSet` WasClaimedBySpecters
      onTrail <- getHasRecord TheInvestigatorsAreOnJerome'sTrail

      location <- getJustLocation iid

      if disappearedIntoTheMist && onTrail
        then do
          jerome <- getSetAsideCard Assets.jeromeDavids
          enabled <- createCardEffect Cards.jeromesFate Nothing attrs iid
          pushAll
            [ RemoveStory (toId attrs)
            , enabled
            , TakeControlOfSetAsideAsset iid jerome
            , Record JeromeIsAlive
            ]
        else when claimedBySpecters $ do
          jerome <- genCard Enemies.jeromeDavids
          createJerome <-
            createEnemy jerome
              $ if onTrail
                then toEnemyCreationMethod location
                else toEnemyCreationMethod iid
          pushAll
            [ RemoveStory (toId attrs)
            , toMessage
                $ createJerome
                  { enemyCreationExhausted = onTrail
                  }
            ]

      pure s
    _ -> JeromesFate <$> runMessage msg attrs

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
