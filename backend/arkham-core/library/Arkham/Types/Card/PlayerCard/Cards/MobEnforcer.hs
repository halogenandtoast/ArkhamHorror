module Arkham.Types.Card.PlayerCard.Cards.MobEnforcer where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import qualified Arkham.Types.Keyword as Keyword
import Arkham.Types.Trait

newtype MobEnforcer = MobEnforcer Attrs
  deriving newtype (Show, ToJSON, FromJSON)

mobEnforcer :: CardId -> MobEnforcer
mobEnforcer cardId = MobEnforcer $ (enemy cardId "01101" "Mob Enforcer" 0)
  { pcTraits = [Humanoid, Criminal]
  , pcKeywords = [Keyword.Hunter]
  }
