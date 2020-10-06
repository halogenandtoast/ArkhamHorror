module Arkham.Types.Card.PlayerCard.Cards.BlindingLight2 where

import ClassyPrelude

import Arkham.Json
import qualified Arkham.Types.Action as Action
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Classes.RunMessage
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype BlindingLight2 = BlindingLight2 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance (HasQueue env) => RunMessage env BlindingLight2 where
  runMessage msg (BlindingLight2 attrs) =
    BlindingLight2 <$> runMessage msg attrs

blindingLight2 :: CardId -> BlindingLight2
blindingLight2 cardId = BlindingLight2
  (event cardId "01069" "Blinding Light" 1 Mystic)
    { pcSkills = [SkillWillpower, SkillAgility]
    , pcTraits = [Spell]
    , pcAction = Just Action.Evade
    , pcLevel = 1
    }
