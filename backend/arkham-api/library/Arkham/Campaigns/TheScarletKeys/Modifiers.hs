module Arkham.Campaigns.TheScarletKeys.Modifiers where

import Arkham.Id
import Arkham.Modifier
import Arkham.Prelude

pattern CannotExpose :: ModifierType
pattern CannotExpose <- CampaignModifier "cannotExpose"
  where
    CannotExpose = CampaignModifier "cannotExpose"

pattern NoExposeAt :: ModifierType
pattern NoExposeAt <- CampaignModifier "noExposeAt"
  where
    NoExposeAt = CampaignModifier "noExposeAt"

noExposeAt :: ToId location LocationId => location -> ModifierType
noExposeAt loc = CampaignModifier ("noExposeAt[ " <> tshow (asId loc) <> "]")
