module Arkham.Types.Card.PlayerCard.Cards.DynamiteBlast2 where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Classes.RunMessage
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype DynamiteBlast2 = DynamiteBlast2 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance (HasQueue env) => RunMessage env DynamiteBlast2 where
  runMessage msg (DynamiteBlast2 attrs) =
    DynamiteBlast2 <$> runMessage msg attrs

dynamiteBlast2 :: CardId -> DynamiteBlast2
dynamiteBlast2 cardId = DynamiteBlast2
  (event cardId "50002" "Dynamite Blast" 4 Guardian)
    { pcSkills = [SkillWillpower, SkillCombat]
    , pcTraits = [Tactic]
    , pcAttackOfOpportunityModifiers = [DoesNotProvokeAttacksOfOpportunity]
    , pcLevel = 2
    }
