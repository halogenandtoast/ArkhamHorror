module Arkham.Story.Cards.GavriellasFate (
  GavriellasFate (..),
  gavriellasFate,
  gavriellasFateEffect,
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

newtype GavriellasFate = GavriellasFate StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gavriellasFate :: StoryCard GavriellasFate
gavriellasFate = story GavriellasFate Cards.gavriellasFate

instance RunMessage GavriellasFate where
  runMessage msg s@(GavriellasFate attrs) = case msg of
    ResolveStory iid ResolveIt story' | story' == toId attrs -> do
      disappearedIntoTheMist <-
        toCardCode Investigators.gavriellaMizrah `inRecordSet` DisappearedIntoTheMist
      claimedBySpecters <-
        toCardCode Investigators.gavriellaMizrah `inRecordSet` WasClaimedBySpecters
      onTrail <- getHasRecord TheInvestigatorsAreOnGavriella'sTrail

      location <- getJustLocation iid

      if disappearedIntoTheMist && onTrail
        then do
          gavriella <- getSetAsideCard Assets.gavriellaMizrah
          pushAll
            [ RemoveStory (toId attrs)
            , createCardEffect Cards.gavriellasFate Nothing attrs iid
            , TakeControlOfSetAsideAsset iid gavriella
            , Record GavriellaIsAlive
            ]
        else when claimedBySpecters $ do
          gavriella <- genCard Enemies.gavriellaMizrah
          createGavriella <-
            createEnemy gavriella $
              if onTrail
                then toEnemyCreationMethod location
                else toEnemyCreationMethod iid
          pushAll
            [ RemoveStory (toId attrs)
            , toMessage $
                createGavriella
                  { enemyCreationExhausted = onTrail
                  }
            ]

      pure s
    _ -> GavriellasFate <$> runMessage msg attrs

newtype GavriellasFateEffect = GavriellasFateEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gavriellasFateEffect :: EffectArgs -> GavriellasFateEffect
gavriellasFateEffect = cardEffect GavriellasFateEffect Cards.gavriellasFate

instance HasModifiersFor GavriellasFateEffect where
  getModifiersFor (AssetTarget aid) (GavriellasFateEffect a) = do
    isGavriella <- member aid <$> select (assetIs Assets.gavriellaMizrah)
    pure $ toModifiers a [DoNotTakeUpSlot AllySlot | isGavriella]
  getModifiersFor _ _ = pure []

instance RunMessage GavriellasFateEffect where
  runMessage _ = pure
