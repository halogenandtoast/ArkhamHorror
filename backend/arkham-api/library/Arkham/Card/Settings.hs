{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Arkham.Card.Settings where

import Arkham.Card.CardCode
import Arkham.Prelude
import Control.Lens (non)
import Control.Monad.Fail (fail)
import Data.Data
import Data.Map.Strict qualified as Map

data SetGlobalSetting
  = SetIgnoreUnrelatedSkillTestTriggers Bool
  | FutureProofGlobalSetting
  deriving stock (Show, Eq, Generic, Data)

instance ToJSON SetGlobalSetting
instance FromJSON SetGlobalSetting

data CardSettings = CardSettings
  { globalSettings :: GlobalSettings
  , perCardSettings :: Map CardCode PerCardSettings
  }
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

instance Semigroup CardSettings where
  CardSettings g1 p1 <> CardSettings g2 p2 =
    CardSettings (g1 <> g2) (Map.unionWith (<>) p1 p2)

data GlobalSettings = GlobalSettings
  { ignoreUnrelatedSkillTestTriggers :: Bool
  }
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

instance Semigroup GlobalSettings where
  GlobalSettings i1 <> GlobalSettings i2 =
    GlobalSettings (i1 || i2)

data PerCardSettings = PerCardSettings
  { cardIgnoreUnrelatedSkillTestTriggers :: Bool
  , cardIgnoreDuringSkillTests :: Bool
  , cardAttachments :: [CardCode]
  }
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass ToJSON

instance Semigroup PerCardSettings where
  PerCardSettings i1 d1 a1 <> PerCardSettings i2 d2 a2 =
    PerCardSettings (i1 || i2) (d1 || d2) (a1 <> a2)

instance FromJSON PerCardSettings where
  parseJSON = withObject "PerCardSettings" \o -> do
    cardIgnoreUnrelatedSkillTestTriggers <- o .: "cardIgnoreUnrelatedSkillTestTriggers"
    cardIgnoreDuringSkillTests <- o .: "cardIgnoreDuringSkillTests"
    cardAttachments <- o .:? "cardAttachments" .!= []
    pure PerCardSettings {..}

data PerCardSetting a where
  CardIgnoreUnrelatedSkillTestTriggers :: PerCardSetting Bool
  CardIgnoreDuringSkillTests :: PerCardSetting Bool
  CardAttachments :: PerCardSetting [CardCode]

instance Show (PerCardSetting a) where
  show = \case
    CardIgnoreUnrelatedSkillTestTriggers -> "CardIgnoreUnrelatedSkillTestTriggers"
    CardIgnoreDuringSkillTests -> "CardIgnoreDuringSkillTests"
    CardAttachments -> "CardAttachments"

instance Data (PerCardSetting Bool) where
  gfoldl _ z c = z c
  gunfold _ z c
    | constrIndex c == 1 = z CardIgnoreUnrelatedSkillTestTriggers
    | constrIndex c == 2 = z CardIgnoreDuringSkillTests
    | otherwise = error "gunfold: unknown constructor for PerCardSetting"
  toConstr = \case
    CardIgnoreUnrelatedSkillTestTriggers -> perCardSettingConstr1
    CardIgnoreDuringSkillTests -> perCardSettingConstr2
  dataTypeOf _ = perCardSettingDataType

instance Data (PerCardSetting [CardCode]) where
  gfoldl _ z c = z c
  gunfold _ z c
    | constrIndex c == 3 = z CardAttachments
    | otherwise = error "gunfold: unknown constructor for PerCardSetting"
  toConstr = \case
    CardAttachments -> perCardSettingConstr3
  dataTypeOf _ = perCardSettingDataType

perCardSettingConstr1 :: Constr
perCardSettingConstr1 = mkConstr perCardSettingDataType "CardIgnoreUnrelatedSkillTestTriggers" [] Prefix

perCardSettingConstr2 :: Constr
perCardSettingConstr2 = mkConstr perCardSettingDataType "CardIgnoreDuringSkillTests" [] Prefix

perCardSettingConstr3 :: Constr
perCardSettingConstr3 = mkConstr perCardSettingDataType "CardAttachments" [] Prefix

perCardSettingDataType :: DataType
perCardSettingDataType = mkDataType "PerCardSetting" [perCardSettingConstr1, perCardSettingConstr2]

deriving stock instance Eq (PerCardSetting Bool)
deriving stock instance Ord (PerCardSetting Bool)
deriving stock instance Eq (PerCardSetting [CardCode])
deriving stock instance Ord (PerCardSetting [CardCode])

instance ToJSON (PerCardSetting a) where
  toJSON = String . tshow

data SomePerCardSetting where
  SomePerCardSetting
    :: ( Typeable a
       , Show a
       , Ord a
       , ToJSON a
       , FromJSON a
       , Eq a
       , Eq (PerCardSetting a)
       , Ord (PerCardSetting a)
       , Show (PerCardSetting a)
       , Data a
       , Data (PerCardSetting a)
       )
    => PerCardSetting a
    -> SomePerCardSetting

instance Eq SomePerCardSetting where
  SomePerCardSetting (a :: PerCardSetting a) == SomePerCardSetting (b :: PerCardSetting b) =
    case eqT @a @b of
      Just Refl -> a == b
      Nothing -> False

instance FromJSON SomePerCardSetting where
  parseJSON = withText "SomePerCardSetting" \case
    "CardIgnoreUnrelatedSkillTestTriggers" -> pure $ SomePerCardSetting CardIgnoreUnrelatedSkillTestTriggers
    "CardIgnoreDuringSkillTests" -> pure $ SomePerCardSetting CardIgnoreDuringSkillTests
    "CardAttachments" -> pure $ SomePerCardSetting CardAttachments
    _ -> fail "Unknown PerCardSetting"

data SetCardSetting where
  SetCardSetting
    :: ( Typeable a
       , Show a
       , Ord a
       , Eq a
       , FromJSON a
       , ToJSON a
       , Eq (PerCardSetting a)
       , Ord (PerCardSetting a)
       , Show (PerCardSetting a)
       , Data a
       , Data (PerCardSetting a)
       )
    => PerCardSetting a
    -> a
    -> SetCardSetting

deriving stock instance Show SetCardSetting

instance Eq SetCardSetting where
  SetCardSetting sa (a :: a) == SetCardSetting sb (b :: b) = case eqT @a @b of
    Just Refl -> sa == sb && a == b
    Nothing -> False

instance Ord SetCardSetting where
  compare (SetCardSetting sa (a :: a)) (SetCardSetting sb (b :: b)) = case eqT @a @b of
    Just Refl ->
      case compare sa sb of
        EQ -> compare a b
        ord -> ord
    Nothing -> compare (typeRep sa) (typeRep sb)

instance Data SetCardSetting where
  gfoldl k z (SetCardSetting setting value) = z SetCardSetting `k` setting `k` value

  gunfold _ _ _ = error "gunfold not implemented for SetCardSetting"

  toConstr (SetCardSetting _ _) = setCardSettingConstr

  dataTypeOf _ = setCardSettingDataType

setCardSettingConstr :: Constr
setCardSettingConstr = mkConstr setCardSettingDataType "SetCardSetting" [] Prefix

setCardSettingDataType :: DataType
setCardSettingDataType = mkDataType "SetCardSetting" [setCardSettingConstr]

instance ToJSON SetCardSetting where
  toJSON = \case
    SetCardSetting sa a -> object ["tag" .= sa, "value" .= a]

instance FromJSON SetCardSetting where
  parseJSON = withObject "SetCardSetting" \o -> do
    tag <- o .: "tag"
    case tag of
      SomePerCardSetting sa -> SetCardSetting sa <$> o .: "value"

defaultCardSettings :: CardSettings
defaultCardSettings =
  CardSettings
    { globalSettings =
        GlobalSettings
          { ignoreUnrelatedSkillTestTriggers = True
          }
    , perCardSettings = mempty
    }

defaultPerCardSettings :: PerCardSettings
defaultPerCardSettings =
  PerCardSettings
    { cardIgnoreUnrelatedSkillTestTriggers = False
    , cardIgnoreDuringSkillTests = False
    , cardAttachments = []
    }

globalSettingsL :: Lens' CardSettings GlobalSettings
globalSettingsL = lens globalSettings \m x -> m {globalSettings = x}

ignoreUnrelatedSkillTestTriggersL :: Lens' GlobalSettings Bool
ignoreUnrelatedSkillTestTriggersL =
  lens ignoreUnrelatedSkillTestTriggers \m x -> m {ignoreUnrelatedSkillTestTriggers = x}

updateGlobalSetting :: SetGlobalSetting -> CardSettings -> CardSettings
updateGlobalSetting = \case
  SetIgnoreUnrelatedSkillTestTriggers v ->
    globalSettingsL . ignoreUnrelatedSkillTestTriggersL .~ v
  FutureProofGlobalSetting -> id

perCardSettingsL :: Lens' CardSettings (Map CardCode PerCardSettings)
perCardSettingsL = lens perCardSettings \m x -> m {perCardSettings = x}

perCardSettingsLens :: PerCardSetting a -> Lens' PerCardSettings a
perCardSettingsLens = \case
  CardIgnoreUnrelatedSkillTestTriggers -> cardIgnoreUnrelatedSkillTestTriggersL
  CardIgnoreDuringSkillTests -> cardIgnoreDuringSkillTestsL
  CardAttachments -> cardAttachmentsL

cardIgnoreUnrelatedSkillTestTriggersL :: Lens' PerCardSettings Bool
cardIgnoreUnrelatedSkillTestTriggersL =
  lens cardIgnoreUnrelatedSkillTestTriggers \m x -> m {cardIgnoreUnrelatedSkillTestTriggers = x}

cardIgnoreDuringSkillTestsL :: Lens' PerCardSettings Bool
cardIgnoreDuringSkillTestsL =
  lens cardIgnoreDuringSkillTests \m x -> m {cardIgnoreDuringSkillTests = x}

cardAttachmentsL :: Lens' PerCardSettings [CardCode]
cardAttachmentsL = lens cardAttachments \m x -> m {cardAttachments = x}

updateCardSetting :: CardCode -> SetCardSetting -> CardSettings -> CardSettings
updateCardSetting cCode = \case
  SetCardSetting CardIgnoreUnrelatedSkillTestTriggers v ->
    perCardSettingsL
      . at cCode
      . non defaultPerCardSettings
      . cardIgnoreUnrelatedSkillTestTriggersL
      .~ v
  SetCardSetting CardIgnoreDuringSkillTests v ->
    perCardSettingsL
      . at cCode
      . non defaultPerCardSettings
      . cardIgnoreDuringSkillTestsL
      .~ v
  SetCardSetting CardAttachments v ->
    perCardSettingsL
      . at cCode
      . non defaultPerCardSettings
      . cardAttachmentsL
      .~ v

toPerCardSettings :: [SetCardSetting] -> PerCardSettings
toPerCardSettings = foldr go defaultPerCardSettings
 where
  go :: SetCardSetting -> PerCardSettings -> PerCardSettings
  go (SetCardSetting k v) x = case k of
    CardIgnoreUnrelatedSkillTestTriggers -> x & cardIgnoreUnrelatedSkillTestTriggersL .~ v
    CardIgnoreDuringSkillTests -> x & cardIgnoreDuringSkillTestsL .~ v
    CardAttachments -> x & cardAttachmentsL .~ v
