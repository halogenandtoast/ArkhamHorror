module Arkham.Story.Cards.PennysFate (
  PennysFate (..),
  pennysFate,
  pennysFateEffect,
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

newtype PennysFate = PennysFate StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

pennysFate :: StoryCard PennysFate
pennysFate = story PennysFate Cards.pennysFate

instance RunMessage PennysFate where
  runMessage msg s@(PennysFate attrs) = case msg of
    ResolveStory iid ResolveIt story' | story' == toId attrs -> do
      disappearedIntoTheMist <-
        toCardCode Investigators.pennyWhite `inRecordSet` DisappearedIntoTheMist
      claimedBySpecters <-
        toCardCode Investigators.pennyWhite `inRecordSet` WasClaimedBySpecters
      onTrail <- getHasRecord TheInvestigatorsAreOnPenny'sTrail

      location <- getJustLocation iid

      if disappearedIntoTheMist && onTrail
        then do
          penny <- getSetAsideCard Assets.pennyWhite
          pushAll
            [ RemoveStory (toId attrs)
            , createCardEffect Cards.pennysFate Nothing attrs iid
            , TakeControlOfSetAsideAsset iid penny
            , Record PennyIsAlive
            ]
        else when claimedBySpecters $ do
          penny <- genCard Enemies.pennyWhite
          createPenny <-
            createEnemy penny
              $ if onTrail
                then toEnemyCreationMethod location
                else toEnemyCreationMethod iid
          pushAll
            [ RemoveStory (toId attrs)
            , toMessage
                $ createPenny
                  { enemyCreationExhausted = onTrail
                  }
            ]

      pure s
    _ -> PennysFate <$> runMessage msg attrs

newtype PennysFateEffect = PennysFateEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

pennysFateEffect :: EffectArgs -> PennysFateEffect
pennysFateEffect = cardEffect PennysFateEffect Cards.pennysFate

instance HasModifiersFor PennysFateEffect where
  getModifiersFor (AssetTarget aid) (PennysFateEffect a) = do
    isPenny <- member aid <$> select (assetIs Assets.pennyWhite)
    pure $ toModifiers a [DoNotTakeUpSlot AllySlot | isPenny]
  getModifiersFor _ _ = pure []

instance RunMessage PennysFateEffect where
  runMessage _ = pure
