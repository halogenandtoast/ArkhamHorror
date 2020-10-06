module Arkham.Types.Card.PlayerCard.Cards.Burglary where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype Burglary = Burglary Attrs
  deriving newtype (Show, ToJSON, FromJSON)

burglary :: CardId -> Burglary
burglary cardId = Burglary (asset cardId "01045" "Burglary" 1 Rogue)
  { pcSkills = [SkillIntellect]
  , pcTraits = [Talent, Illicit]
  }
