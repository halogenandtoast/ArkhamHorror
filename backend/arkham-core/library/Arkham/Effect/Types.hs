{-# LANGUAGE TemplateHaskell #-}

module Arkham.Effect.Types where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Classes.Entity
import Arkham.Classes.HasAbilities
import Arkham.Classes.HasModifiersFor
import Arkham.Classes.RunMessage.Internal
import Arkham.Effect.Window
import Arkham.EffectMetadata
import {-# SOURCE #-} Arkham.Helpers.Calculation (calculate)
import Arkham.Id
import Arkham.Json
import Arkham.Message
import Arkham.Modifier
import Arkham.Projection
import Arkham.Source
import Arkham.Target
import Arkham.Trait
import Arkham.Window (Window)
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
  ) =>
  IsEffect a

data instance Field Effect :: Type -> Type where
  EffectCardCode :: Field Effect CardCode
  EffectAbilities :: Field Effect [Ability]
  EffectMeta :: Field Effect (Maybe (EffectMetadata Window Message))

cardEffect :: (EffectAttrs -> a) -> CardDef -> EffectArgs -> a
cardEffect f def = f . uncurry4 (baseAttrs (toCardCode def))

cardEffectWith :: (EffectAttrs -> a) -> CardDef -> (EffectAttrs -> EffectAttrs) -> EffectArgs -> a
cardEffectWith f def g = f . g . uncurry4 (baseAttrs (toCardCode def))

instance AsId EffectAttrs where
  type IdOf EffectAttrs = EffectId
  asId = toId

data EffectAttrs = EffectAttrs
  { effectId :: EffectId
  , effectCardCode :: CardCode
  , effectTarget :: Target
  , effectSource :: Source
  , effectTraits :: Set Trait
  , effectMetadata :: Maybe (EffectMetadata Window Message)
  , effectWindow :: Maybe EffectWindow
  , effectFinished :: Bool
  -- ^ Sometimes an effect may cause infinite recursion, this bool can be used
  -- to track and escape recursion
  , effectExtraMetadata :: Value
  , effectSkillTest :: Maybe SkillTestId
  }
  deriving stock (Show, Eq, Data)

replaceNextSkillTest :: SkillTestId -> EffectAttrs -> EffectAttrs
replaceNextSkillTest sid e = e {effectWindow = replaceNextSkillTestWindow e.window}
 where
  replaceNextSkillTestWindow = \case
    Just EffectNextSkillTestWindow -> Just $ EffectSkillTestWindow sid
    a -> a

instance HasCardCode EffectAttrs where
  toCardCode = effectCardCode

instance HasField "id" EffectAttrs EffectId where
  getField = effectId

instance HasField "window" EffectAttrs (Maybe EffectWindow) where
  getField = effectWindow

instance HasField "window" Effect (Maybe EffectWindow) where
  getField = effectWindow . toAttrs

instance HasField "skillTest" Effect (Maybe SkillTestId) where
  getField = effectSkillTest . toAttrs

finishedL :: Lens' EffectAttrs Bool
finishedL = lens effectFinished $ \m x -> m {effectFinished = x}

extraL :: Lens' EffectAttrs Value
extraL = lens effectExtraMetadata $ \m x -> m {effectExtraMetadata = x}

instance HasField "finished" EffectAttrs Bool where
  getField = effectFinished

instance HasField "target" EffectAttrs Target where
  getField = effectTarget

instance HasField "source" EffectAttrs Source where
  getField = effectSource

instance HasField "metadata" EffectAttrs (Maybe (EffectMetadata Window Message)) where
  getField = effectMetadata

instance HasField "meta" EffectAttrs (Maybe (EffectMetadata Window Message)) where
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

type EffectArgs =
  (EffectId, Maybe (EffectMetadata Window Message), Source, Target)

baseAttrs
  :: CardCode
  -> EffectId
  -> Maybe (EffectMetadata Window Message)
  -> Source
  -> Target
  -> EffectAttrs
baseAttrs cardCode eid meffectMetadata source target =
  EffectAttrs
    { effectId = eid
    , effectSource = source
    , effectTarget = target
    , effectCardCode = cardCode
    , effectMetadata = meffectMetadata
    , effectTraits = mempty
    , effectWindow = Nothing
    , effectFinished = False
    , effectExtraMetadata = Null
    , effectSkillTest = Nothing
    }

targetL :: Lens' EffectAttrs Target
targetL = lens effectTarget $ \m x -> m {effectTarget = x}

metadataL :: Lens' EffectAttrs (Maybe (EffectMetadata Window Message))
metadataL = lens effectMetadata $ \m x -> m {effectMetadata = x}

instance HasAbilities EffectAttrs

isEndOfWindow :: EffectAttrs -> EffectWindow -> Bool
isEndOfWindow EffectAttrs {effectWindow} effectWindow' =
  effectWindow'
    `elem` toEffectWindowList effectWindow
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
  getModifiersFor target (Effect a) = do
    mods <- getModifiersFor target a
    foldMapM expandForEach mods
   where
    expandForEach x@(modifierType -> ForEach calc ms) = do
      n <- calculate calc
      pure $ map (\m -> x {modifierType = m}) (concat @[[ModifierType]] $ replicate n ms)
    expandForEach m = pure [m]

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

$(deriveJSON (aesonOptions $ Just "effect") ''EffectAttrs)
