module Arkham.Types.Card.PlayerCard.Cards.DynamiteBlast where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Classes.RunMessage
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype DynamiteBlast = DynamiteBlast Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance (HasQueue env) => RunMessage env DynamiteBlast where
  runMessage msg (DynamiteBlast attrs) = DynamiteBlast <$> runMessage msg attrs

dynamiteBlast :: CardId -> DynamiteBlast
dynamiteBlast cardId = DynamiteBlast
  (event cardId "01024" "Dynamite Blast" 5 Guardian)
    { pcSkills = [SkillWillpower]
    , pcTraits = [Tactic]
    }
