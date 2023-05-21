{-# LANGUAGE TemplateHaskell #-}

module Arkham.Story.Types where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes.Entity
import Arkham.Classes.HasAbilities
import Arkham.Classes.HasModifiersFor
import Arkham.Classes.RunMessage.Internal
import Arkham.Id
import Arkham.Json
import Arkham.Name
import Arkham.Placement
import Arkham.Projection
import Arkham.Source
import Arkham.Story.Cards
import Arkham.Target
import Data.Typeable

class
  ( Typeable a
  , ToJSON a
  , FromJSON a
  , Eq a
  , Show a
  , HasAbilities a
  , HasModifiersFor a
  , RunMessage a
  , Entity a
  , EntityId a ~ StoryId
  , EntityAttrs a ~ StoryAttrs
  ) =>
  IsStory a

type StoryCard a = CardBuilder (Maybe Target, StoryId) a

data instance Field Story :: Type -> Type where
  StoryCard :: Field Story Card
  StoryPlacement :: Field Story Placement
  StoryOtherSide :: Field Story (Maybe Target)

data StoryAttrs = StoryAttrs
  { storyId :: StoryId
  , storyCardId :: CardId
  , storyPlacement :: Placement
  , storyOtherSide :: Maybe Target
  }
  deriving stock (Show, Eq, Generic)

storyWith
  :: (StoryAttrs -> a)
  -> CardDef
  -> (StoryAttrs -> StoryAttrs)
  -> CardBuilder (Maybe Target, StoryId) a
storyWith f cardDef g =
  CardBuilder
    { cbCardCode = cdCardCode cardDef
    , cbCardBuilder = \cardId (mTarget, sid) ->
        f . g $
          StoryAttrs
            { storyId = sid
            , storyCardId = cardId
            , storyPlacement = Unplaced
            , storyOtherSide = mTarget
            }
    }

story
  :: (StoryAttrs -> a)
  -> CardDef
  -> CardBuilder (Maybe Target, StoryId) a
story f cardDef = storyWith f cardDef id

instance HasCardDef StoryAttrs where
  toCardDef e = case lookup (unStoryId $ storyId e) allStoryCards of
    Just def -> def
    Nothing -> error $ "missing card def for story " <> show (unStoryId $ storyId e)

instance ToJSON StoryAttrs where
  toJSON = genericToJSON $ aesonOptions $ Just "story"
  toEncoding = genericToEncoding $ aesonOptions $ Just "story"

instance FromJSON StoryAttrs where
  parseJSON = genericParseJSON $ aesonOptions $ Just "story"

instance Entity StoryAttrs where
  type EntityId StoryAttrs = StoryId
  type EntityAttrs StoryAttrs = StoryAttrs
  toId = storyId
  toAttrs = id
  overAttrs f = f

instance Named Story where
  toName = toName . toAttrs

instance Named StoryAttrs where
  toName = toName . toCardDef

instance Targetable StoryAttrs where
  toTarget = StoryTarget . toId
  isTarget StoryAttrs {storyId} (StoryTarget sid) = storyId == sid
  isTarget _ _ = False

instance Sourceable StoryAttrs where
  toSource = StorySource . toId
  isSource StoryAttrs {storyId} (StorySource sid) = storyId == sid
  isSource _ _ = False

data Story = forall a. (IsStory a) => Story a

instance Eq Story where
  (Story (a :: a)) == (Story (b :: b)) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

instance Show Story where
  show (Story a) = show a

instance ToJSON Story where
  toJSON (Story a) = toJSON a

instance HasAbilities Story where
  getAbilities (Story a) = getAbilities a

instance HasModifiersFor Story where
  getModifiersFor target (Story a) = getModifiersFor target a

instance Entity Story where
  type EntityId Story = StoryId
  type EntityAttrs Story = StoryAttrs
  toId = toId . toAttrs
  toAttrs (Story a) = toAttrs a
  overAttrs f (Story a) = Story $ overAttrs f a

instance Targetable Story where
  toTarget = toTarget . toAttrs
  isTarget = isTarget . toAttrs

instance Sourceable Story where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

instance HasCardCode StoryAttrs where
  toCardCode = unStoryId . storyId

instance IsCard StoryAttrs where
  toCardId = storyCardId
  toCard a = lookupCard (unStoryId $ storyId a) (toCardId a)

data SomeStoryCard = forall a. (IsStory a) => SomeStoryCard (StoryCard a)

liftSomeStoryCard :: (forall a. StoryCard a -> b) -> SomeStoryCard -> b
liftSomeStoryCard f (SomeStoryCard a) = f a

someStoryCardCode :: SomeStoryCard -> CardCode
someStoryCardCode = liftSomeStoryCard cbCardCode

makeLensesWith suffixedFields ''StoryAttrs
