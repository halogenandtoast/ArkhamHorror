{-# LANGUAGE TemplateHaskell #-}

module Arkham.Effect.Types where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Classes.Entity
import Arkham.Classes.HasAbilities
import Arkham.Classes.HasGame
import Arkham.Classes.HasModifiersFor
import Arkham.Classes.RunMessage.Internal
import Arkham.Effect.Window
import Arkham.EffectMetadata
import {-# SOURCE #-} Arkham.Helpers.Ref
import Arkham.Id
import Arkham.Json
import Arkham.Message
import Arkham.Projection
import Arkham.Source
import Arkham.Target
import Arkham.Trait
import Data.Aeson.TH
import Data.Data
import GHC.Records

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
  , EntityId a ~ EffectId
  , EntityAttrs a ~ EffectAttrs
  , RunType a ~ a
  ) =>
  IsEffect a

data instance Field Effect :: Type -> Type where
  EffectCardCode :: Field Effect CardCode
  EffectCard :: Field Effect (Maybe Card)
  EffectAbilities :: Field Effect [Ability]
  EffectMeta :: Field Effect (Maybe (EffectMetadata Message))

cardEffect :: (EffectAttrs -> a) -> CardDef -> EffectArgs -> a
cardEffect f def = f . uncurry (baseAttrs (toCardCode def))

cardEffectWith :: (EffectAttrs -> a) -> CardDef -> (EffectAttrs -> EffectAttrs) -> EffectArgs -> a
cardEffectWith f def g = f . g . uncurry (baseAttrs (toCardCode def))

instance AsId EffectAttrs where
  type IdOf EffectAttrs = EffectId
  asId = toId

data EffectBuilder = EffectBuilder
  { effectBuilderCardCode :: CardCode
  , effectBuilderTarget :: Target
  , effectBuilderSource :: Source
  , effectBuilderTraits :: Set Trait
  , effectBuilderMetadata :: Maybe (EffectMetadata Message)
  , effectBuilderWindow :: Maybe EffectWindow
  , effectBuilderDisableWindow :: Maybe EffectWindow
  , effectBuilderOnDisable :: Maybe [Message]
  , effectBuilderFinished :: Bool
  {- ^ Sometimes an effect may cause infinite recursion, this bool can be used
  to track and escape recursion
  -}
  , effectBuilderExtraMetadata :: Value
  , effectBuilderSkillTest :: Maybe SkillTestId
  , effectBuilderEffectId :: Maybe EffectId
  , effectBuilderCardId :: Maybe CardId
  }
  deriving stock (Show, Eq, Data)

data EffectAttrs = EffectAttrs
  { effectId :: EffectId
  , effectCardCode :: CardCode
  , effectTarget :: Target
  , effectSource :: Source
  , effectCardId :: Maybe CardId
  , effectTraits :: Set Trait
  , effectMetadata :: Maybe (EffectMetadata Message)
  , effectWindow :: Maybe EffectWindow
  , effectDisableWindow :: Maybe EffectWindow
  , effectFinished :: Bool
  {- ^ Sometimes an effect may cause infinite recursion, this bool can be used
  to track and escape recursion
  -}
  , effectExtraMetadata :: Value
  , effectSkillTest :: Maybe SkillTestId
  , effectMetaKeys :: [Text]
  , effectOnDisable :: Maybe [Message]
  }
  deriving stock (Show, Eq, Data)

replaceNextSkillTest :: SkillTestId -> InvestigatorId -> EffectAttrs -> EffectAttrs
replaceNextSkillTest sid iid e = e {effectWindow = replaceNextSkillTestWindow e.window}
 where
  replaceNextSkillTestWindow = \case
    Just (EffectNextSkillTestWindow iid') | iid == iid' -> Just $ EffectSkillTestWindow sid
    a -> a

instance HasCardCode EffectAttrs where
  toCardCode = effectCardCode

instance HasField "id" EffectAttrs EffectId where
  getField = effectId

instance HasField "window" EffectAttrs (Maybe EffectWindow) where
  getField = effectWindow

instance HasField "skillTest" EffectAttrs (Maybe SkillTestId) where
  getField = effectSkillTest

instance HasField "window" Effect (Maybe EffectWindow) where
  getField = effectWindow . toAttrs

instance HasField "skillTest" Effect (Maybe SkillTestId) where
  getField = effectSkillTest . toAttrs

instance HasField "source" Effect Source where
  getField = effectSource . toAttrs

instance HasField "target" Effect Target where
  getField = effectTarget . toAttrs

instance HasField "metadata" Effect (Maybe (EffectMetadata Message)) where
  getField = effectMetadata . toAttrs

finishedL :: Lens' EffectAttrs Bool
finishedL = lens effectFinished $ \m x -> m {effectFinished = x}

extraL :: Lens' EffectAttrs Value
extraL = lens effectExtraMetadata $ \m x -> m {effectExtraMetadata = x}

metaKeysL :: Lens' EffectAttrs [Text]
metaKeysL = lens effectMetaKeys $ \m x -> m {effectMetaKeys = x}

instance HasField "finished" EffectAttrs Bool where
  getField = effectFinished

instance HasField "target" EffectAttrs Target where
  getField = effectTarget

instance HasField "source" EffectAttrs Source where
  getField = effectSource

instance HasField "metadata" EffectAttrs (Maybe (EffectMetadata Message)) where
  getField = effectMetadata

instance HasField "meta" EffectAttrs (Maybe (EffectMetadata Message)) where
  getField = effectMetadata

instance HasField "metaTarget" EffectAttrs (Maybe Target) where
  getField e = case e.meta of
    Just (EffectMetaTarget t) -> Just t
    _ -> Nothing

instance HasField "metaInt" EffectAttrs (Maybe Int) where
  getField e = case e.meta of
    Just (EffectInt t) -> Just t
    _ -> Nothing

instance HasField "extra" EffectAttrs Value where
  getField = effectExtraMetadata

instance HasField "metaKeys" EffectAttrs [Text] where
  getField = effectMetaKeys

type EffectArgs = (EffectId, EffectBuilder)

baseAttrs
  :: CardCode
  -> EffectId
  -> EffectBuilder
  -> EffectAttrs
baseAttrs cardCode eid EffectBuilder {..} =
  EffectAttrs
    { effectId = eid
    , effectSource = effectBuilderSource
    , effectTarget = effectBuilderTarget
    , effectCardCode = cardCode
    , effectMetadata = effectBuilderMetadata
    , effectTraits = effectBuilderTraits
    , effectWindow = effectBuilderWindow
    , effectDisableWindow = effectBuilderDisableWindow
    , effectOnDisable = effectBuilderOnDisable
    , effectFinished = effectBuilderFinished
    , effectExtraMetadata = effectBuilderExtraMetadata
    , effectSkillTest = effectBuilderSkillTest
    , effectCardId = effectBuilderCardId
    , effectMetaKeys = []
    }

targetL :: Lens' EffectAttrs Target
targetL = lens effectTarget $ \m x -> m {effectTarget = x}

metadataL :: Lens' EffectAttrs (Maybe (EffectMetadata Message))
metadataL = lens effectMetadata $ \m x -> m {effectMetadata = x}

instance HasAbilities EffectAttrs

isEndOfWindow :: EffectAttrs -> EffectWindow -> Bool
isEndOfWindow EffectAttrs {effectWindow, effectDisableWindow} effectWindow' =
  effectWindow' `elem` toEffectWindowList (effectDisableWindow <|> effectWindow)
 where
  toEffectWindowList Nothing = []
  toEffectWindowList (Just (FirstEffectWindow xs)) = xs
  toEffectWindowList (Just x) = [x]

instance Entity EffectAttrs where
  type EntityId EffectAttrs = EffectId
  type EntityAttrs EffectAttrs = EffectAttrs
  toId = effectId
  toAttrs = id
  overAttrs f = f

instance Targetable EffectAttrs where
  toTarget = EffectTarget . toId
  isTarget EffectAttrs {effectId} (EffectTarget eid) = effectId == eid
  isTarget _ _ = False

instance Sourceable EffectAttrs where
  toSource = EffectSource . toId
  isSource EffectAttrs {effectId} (EffectSource eid) = effectId == eid
  isSource _ _ = False

data Effect = forall a. IsEffect a => Effect a

instance Data Effect where
  gunfold _ _ _ = error "gunfold(Effect)"
  toConstr _ = error "toConstr(Effect)"
  dataTypeOf _ = error "dataTypeOf(Effect)"

instance Eq Effect where
  (Effect (a :: a)) == (Effect (b :: b)) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

instance Show Effect where
  show (Effect a) = show a

instance ToJSON Effect where
  toJSON (Effect a) = toJSON a

instance HasModifiersFor Effect where
  getModifiersFor (Effect a) =
    unless (effectFinished (toAttrs a)) $ getModifiersFor a

instance HasAbilities Effect where
  getAbilities (Effect a) = getAbilities a

instance Entity Effect where
  type EntityId Effect = EffectId
  type EntityAttrs Effect = EffectAttrs
  toId = toId . toAttrs
  toAttrs (Effect a) = toAttrs a
  overAttrs f (Effect a) = Effect $ overAttrs f a

instance Targetable Effect where
  toTarget = toTarget . toAttrs
  isTarget = isTarget . toAttrs

instance Sourceable Effect where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

data SomeEffect = forall a. IsEffect a => SomeEffect (EffectArgs -> a)

disableEffect :: (AsId a, IdOf a ~ EffectId) => a -> Message
disableEffect = DisableEffect . asId

disable :: (AsId a, IdOf a ~ EffectId) => a -> Message
disable = disableEffect

setEffectMeta :: ToJSON a => a -> EffectAttrs -> EffectAttrs
setEffectMeta a = extraL .~ toJSON a

makeEffectBuilder
  :: (Sourceable source, Targetable target, HasGame m)
  => CardCode
  -> Maybe (EffectMetadata Message)
  -> source
  -> target
  -> m EffectBuilder
makeEffectBuilder cardCode meffectMetadata rawSource@(toSource -> source) (toTarget -> target) = do
  mcard <- sourceToMaybeCard rawSource
  pure
    $ EffectBuilder
      { effectBuilderSource = source
      , effectBuilderTarget = target
      , effectBuilderCardCode = cardCode
      , effectBuilderMetadata = meffectMetadata
      , effectBuilderTraits = mempty
      , effectBuilderWindow = Nothing
      , effectBuilderDisableWindow = Nothing
      , effectBuilderOnDisable = Nothing
      , effectBuilderFinished = False
      , effectBuilderExtraMetadata = Null
      , effectBuilderSkillTest = mSkillTest
      , effectBuilderEffectId = Nothing
      , effectBuilderCardId = toCardId <$> mcard
      }
 where
  mSkillTest = case (meffectMetadata, source, target, effectWindow) of
    (Just (EffectMetaTarget (SkillTestTarget sid)), _, _, _) -> Just sid
    (_, SkillTestSource sid, _, _) -> Just sid
    (_, _, SkillTestTarget sid, _) -> Just sid
    _ -> Nothing

$(deriveJSON (aesonOptions $ Just "effectBuilder") ''EffectBuilder)
$(deriveToJSON (aesonOptions $ Just "effect") ''EffectAttrs)

instance FromJSON EffectAttrs where
  parseJSON = withObject "EffectAttrs" \o -> do
    effectId <- o .: "id"
    effectCardCode <- o .: "cardCode"
    effectTarget <- o .: "target"
    effectSource <- o .: "source"
    effectCardId <- o .:? "cardId"
    effectTraits <- o .: "traits"
    effectMetadata <- o .: "metadata"
    effectWindow <- o .: "window"
    effectDisableWindow <- o .:? "disableWindow"
    effectFinished <- o .: "finished"
    effectExtraMetadata <- o .: "extraMetadata"
    effectSkillTest <- o .: "skillTest"
    effectMetaKeys <- o .:? "metaKeys" .!= []
    effectOnDisable <- o .:? "onDisable"
    pure EffectAttrs {..}
