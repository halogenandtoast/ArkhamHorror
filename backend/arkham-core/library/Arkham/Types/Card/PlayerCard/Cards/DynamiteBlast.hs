module Arkham.Types.Card.PlayerCard.Cards.DynamiteBlast where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype DynamiteBlast = DynamiteBlast Attrs
  deriving newtype (Show, ToJSON, FromJSON)

dynamiteBlast :: CardId -> DynamiteBlast
dynamiteBlast cardId = DynamiteBlast
  (event cardId "01024" "Dynamite Blast" 5 Guardian)
    { pcSkills = [SkillWillpower]
    , pcTraits = [Tactic]
    }
