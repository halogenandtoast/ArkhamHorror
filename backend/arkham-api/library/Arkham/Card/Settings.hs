module Arkham.Card.Settings where

import Arkham.Card.CardCode
import Arkham.Prelude
import Control.Lens (non)

data SetGlobalSetting
  = SetIgnoreUnrelatedSkillTestReactions Bool
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
  { ignoreUnrelatedSkillTestReactions :: Bool
  }
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

data PerCardSettings = PerCardSettings
  { cardIgnoreUnrelatedSkillTestReactions :: Bool
  }
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

data PerCardSetting a where
  CardIgnoreUnrelatedSkillTestReactions :: PerCardSetting Bool

data SetCardSetting
  = SetCardIgnoreUnrelatedSkillTestReactions Bool
  deriving stock (Show, Eq, Generic, Data)

instance ToJSON SetCardSetting
instance FromJSON SetCardSetting

defaultCardSettings :: CardSettings
defaultCardSettings =
  CardSettings
    { globalSettings =
        GlobalSettings
          { ignoreUnrelatedSkillTestReactions = True
          }
    , perCardSettings = mempty
    }

defaultPerCardSettings :: PerCardSettings
defaultPerCardSettings =
  PerCardSettings
    { cardIgnoreUnrelatedSkillTestReactions = False
    }

globalSettingsL :: Lens' CardSettings GlobalSettings
globalSettingsL = lens globalSettings \m x -> m {globalSettings = x}

ignoreUnrelatedSkillTestReactionsL :: Lens' GlobalSettings Bool
ignoreUnrelatedSkillTestReactionsL =
  lens ignoreUnrelatedSkillTestReactions \m x -> m {ignoreUnrelatedSkillTestReactions = x}

updateGlobalSetting :: SetGlobalSetting -> CardSettings -> CardSettings
updateGlobalSetting = \case
  SetIgnoreUnrelatedSkillTestReactions v ->
    globalSettingsL . ignoreUnrelatedSkillTestReactionsL .~ v

perCardSettingsL :: Lens' CardSettings (Map CardCode PerCardSettings)
perCardSettingsL = lens perCardSettings \m x -> m {perCardSettings = x}

perCardSettingsLens :: PerCardSetting a -> Lens' PerCardSettings a
perCardSettingsLens = \case
  CardIgnoreUnrelatedSkillTestReactions -> lens cardIgnoreUnrelatedSkillTestReactions \m x -> m {cardIgnoreUnrelatedSkillTestReactions = x}

cardIgnoreUnrelatedSkillTestReactionsL :: Lens' PerCardSettings Bool
cardIgnoreUnrelatedSkillTestReactionsL =
  lens cardIgnoreUnrelatedSkillTestReactions \m x -> m {cardIgnoreUnrelatedSkillTestReactions = x}

updateCardSetting :: CardCode -> SetCardSetting -> CardSettings -> CardSettings
updateCardSetting cCode = \case
  SetCardIgnoreUnrelatedSkillTestReactions v ->
    perCardSettingsL
      . at cCode
      . non defaultPerCardSettings
      . cardIgnoreUnrelatedSkillTestReactionsL
      .~ v
