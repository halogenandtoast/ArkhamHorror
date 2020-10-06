module Arkham.Types.Card.PlayerCard.Cards.JimsTrumpet where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype JimsTrumpet = JimsTrumpet Attrs
  deriving newtype (Show, ToJSON, FromJSON)

jimsTrumpet :: CardId -> JimsTrumpet
jimsTrumpet cardId =
  JimsTrumpet $ (asset cardId "02012" "Jim's Trumpet" 2 Neutral)
    { pcSkills = [SkillWillpower, SkillWillpower, SkillWild]
    , pcTraits = [Item, Instrument, Relic]
    }
