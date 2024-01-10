{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Story (
  Story (..),
  createStory,
  lookupStory,
) where

import Arkham.Prelude hiding (fold)

import Arkham.Card
import Arkham.Id
import Arkham.Story.Stories
import Arkham.Story.Types
import Arkham.Target

createStory :: IsCard a => a -> Maybe Target -> StoryId -> Story
createStory a mtarget sId = lookupStory sId mtarget (toCardId a)

lookupStory :: StoryId -> Maybe Target -> CardId -> Story
lookupStory storyId = case lookup (unStoryId storyId) allStories of
  Nothing -> error $ "Unknown story: " <> show storyId
  Just (SomeStoryCard a) -> \mtarget cardId -> Story $ cbCardBuilder a cardId (mtarget, storyId)

instance FromJSON Story where
  parseJSON = withObject "Story" $ \o -> do
    cCode <- o .: "id"
    withStoryCardCode cCode $ \(_ :: StoryCard a) -> Story <$> parseJSON @a (Object o)

withStoryCardCode :: CardCode -> (forall a. IsStory a => StoryCard a -> r) -> r
withStoryCardCode cCode f = case lookup cCode allStories of
  Nothing -> error $ "Unknown story: " <> show cCode
  Just (SomeStoryCard a) -> f a

allStories :: Map CardCode SomeStoryCard
allStories =
  mapFrom
    someStoryCardCode
    [ -- The Path to Carcosa
      -- The Last King
      SomeStoryCard sickeningReality_65
    , SomeStoryCard sickeningReality_66
    , SomeStoryCard sickeningReality_67
    , SomeStoryCard sickeningReality_68
    , SomeStoryCard sickeningReality_69
    , SomeStoryCard engramsOath
    , SomeStoryCard lagneauPerdu
    , SomeStoryCard thePattern
    , SomeStoryCard theFirstShow
    , SomeStoryCard aboveAndBelow
    , SomeStoryCard songsThatTheHyadesShallSing
    , SomeStoryCard starsOfAldebaran
    , SomeStoryCard bleakDesolation
    , SomeStoryCard inhabitantOfCarcosa
    , SomeStoryCard aMomentsRest
    , SomeStoryCard theCoffin
    , SomeStoryCard mappingTheStreets
    , SomeStoryCard theKingsParade
    , SomeStoryCard theArchway
    , SomeStoryCard theHeightOfTheDepths
    , SomeStoryCard stepsOfThePalace
    , SomeStoryCard theFall
    , SomeStoryCard hastursEnd
    , SomeStoryCard yigsMercy
    , SomeStoryCard josefsPlan
    , SomeStoryCard unfinishedBusiness_B
    , SomeStoryCard unfinishedBusiness_D
    , SomeStoryCard unfinishedBusiness_F
    , SomeStoryCard unfinishedBusiness_H
    , SomeStoryCard unfinishedBusiness_J
    , SomeStoryCard unfinishedBusiness_L
    , SomeStoryCard gavriellasFate
    , SomeStoryCard jeromesFate
    , SomeStoryCard pennysFate
    , SomeStoryCard valentinosFate
    , SomeStoryCard theTrialOfKamanThah
    , SomeStoryCard theTrialOfNasht
    , SomeStoryCard theInfestationBegins
    , SomeStoryCard crypticSouls
    , SomeStoryCard dreamlikeHorrors
    , SomeStoryCard endlessSecrets
    , SomeStoryCard cylindersOfKadatheron
    , SomeStoryCard theDoomOfSarnath
    , SomeStoryCard ghostsOfTheDead
    , SomeStoryCard thePalaceOfRainbows
    , SomeStoryCard aShrineToTheGods
    , SomeStoryCard theCryptOfZulanThek
    , SomeStoryCard waresOfBaharna
    ]
