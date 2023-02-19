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
import Arkham.Id
import Arkham.Json
import Arkham.Message
import Arkham.Projection
import Arkham.Source
import Arkham.Target
import Arkham.Trait
import Arkham.Window ( Window )
import Data.Typeable

class (Typeable a, ToJSON a, FromJSON a, Eq a, Show a, HasAbilities a, HasModifiersFor a, RunMessage a, Entity a, EntityId a ~ EffectId, EntityAttrs a ~ EffectAttrs) => IsEffect a

data instance Field Effect :: Type -> Type where
  EffectAbilities :: Field Effect [Ability]

cardEffect :: (EffectAttrs -> a) -> CardDef -> EffectArgs -> a
cardEffect f def = f . uncurry4 (baseAttrs (toCardCode def))

data EffectAttrs = EffectAttrs
  { effectId :: EffectId
  , effectCardCode :: CardCode
  , effectTarget :: Target
  , effectSource :: Source
  , effectTraits :: HashSet Trait
  , effectMetadata :: Maybe (EffectMetadata Window Message)
  , effectWindow :: Maybe EffectWindow
  , effectFinished :: Bool
  -- ^ Sometimes an effect may cause infinite recursion, this bool can be used
  -- to track and escape recursion
  }
  deriving stock (Show, Eq, Generic)

finishedL :: Lens' EffectAttrs Bool
finishedL = lens effectFinished $ \m x -> m { effectFinished = x }

type EffectArgs
  = (EffectId, Maybe (EffectMetadata Window Message), Source, Target)

baseAttrs
  :: CardCode
  -> EffectId
  -> Maybe (EffectMetadata Window Message)
  -> Source
  -> Target
  -> EffectAttrs
baseAttrs cardCode eid meffectMetadata source target = EffectAttrs
  { effectId = eid
  , effectSource = source
  , effectTarget = target
  , effectCardCode = cardCode
  , effectMetadata = meffectMetadata
  , effectTraits = mempty
  , effectWindow = Nothing
  , effectFinished = False
  }

targetL :: Lens' EffectAttrs Target
targetL = lens effectTarget $ \m x -> m { effectTarget = x }

instance ToJSON EffectAttrs where
  toJSON = genericToJSON $ aesonOptions $ Just "effect"
  toEncoding = genericToEncoding $ aesonOptions $ Just "effect"

instance FromJSON EffectAttrs where
  parseJSON = genericParseJSON $ aesonOptions $ Just "effect"

instance HasAbilities EffectAttrs

isEndOfWindow :: EffectAttrs -> EffectWindow -> Bool
isEndOfWindow EffectAttrs { effectWindow } effectWindow' = effectWindow'
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

instance TargetEntity EffectAttrs where
  toTarget = EffectTarget . toId
  isTarget EffectAttrs { effectId } (EffectTarget eid) = effectId == eid
  isTarget _ _ = False

instance SourceEntity EffectAttrs where
  toSource = EffectSource . toId
  isSource EffectAttrs { effectId } (EffectSource eid) = effectId == eid
  isSource _ _ = False

data Effect = forall a . IsEffect a => Effect a

instance Eq Effect where
  (Effect (a :: a)) == (Effect (b :: b)) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

instance Show Effect where
  show (Effect a) = show a

instance ToJSON Effect where
  toJSON (Effect a) = toJSON a

instance HasModifiersFor Effect where
  getModifiersFor target (Effect a) = getModifiersFor target a

instance HasAbilities Effect where
  getAbilities (Effect a) = getAbilities a

instance Entity Effect where
  type EntityId Effect = EffectId
  type EntityAttrs Effect = EffectAttrs
  toId = toId . toAttrs
  toAttrs (Effect a) = toAttrs a
  overAttrs f (Effect a) = Effect $ overAttrs f a

instance TargetEntity Effect where
  toTarget = toTarget . toAttrs
  isTarget = isTarget . toAttrs

instance SourceEntity Effect where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

data SomeEffect = forall a . IsEffect a => SomeEffect (EffectArgs -> a)

