module Arkham.Card.Settings where

import Arkham.Card.CardCode
import Arkham.Prelude
import Control.Lens (non)

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

data GlobalSettings = GlobalSettings
  { ignoreUnrelatedSkillTestTriggers :: Bool
  }
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

data PerCardSettings = PerCardSettings
  { cardIgnoreUnrelatedSkillTestTriggers :: Bool
  }
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

data PerCardSetting a where
  CardIgnoreUnrelatedSkillTestTriggers :: PerCardSetting Bool

data SetCardSetting
  = SetCardIgnoreUnrelatedSkillTestTriggers Bool
  deriving stock (Show, Eq, Generic, Data)

instance ToJSON SetCardSetting
instance FromJSON SetCardSetting

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
  CardIgnoreUnrelatedSkillTestTriggers -> lens cardIgnoreUnrelatedSkillTestTriggers \m x -> m {cardIgnoreUnrelatedSkillTestTriggers = x}

cardIgnoreUnrelatedSkillTestTriggersL :: Lens' PerCardSettings Bool
cardIgnoreUnrelatedSkillTestTriggersL =
  lens cardIgnoreUnrelatedSkillTestTriggers \m x -> m {cardIgnoreUnrelatedSkillTestTriggers = x}

updateCardSetting :: CardCode -> SetCardSetting -> CardSettings -> CardSettings
updateCardSetting cCode = \case
  SetCardIgnoreUnrelatedSkillTestTriggers v ->
    perCardSettingsL
      . at cCode
      . non defaultPerCardSettings
      . cardIgnoreUnrelatedSkillTestTriggersL
      .~ v
