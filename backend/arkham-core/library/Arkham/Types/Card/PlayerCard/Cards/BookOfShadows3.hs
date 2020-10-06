module Arkham.Types.Card.PlayerCard.Cards.BookOfShadows3 where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Classes.RunMessage
import Arkham.Types.ClassSymbol
import Arkham.Types.SkillType
import Arkham.Types.Trait

newtype BookOfShadows3 = BookOfShadows3 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance (HasQueue env) => RunMessage env BookOfShadows3 where
  runMessage msg (BookOfShadows3 attrs) =
    BookOfShadows3 <$> runMessage msg attrs

bookOfShadows3 :: CardId -> BookOfShadows3
bookOfShadows3 cardId = BookOfShadows3
  (asset cardId "01070" "Book of Shadows" 4 Mystic)
    { pcSkills = [SkillWillpower, SkillIntellect]
    , pcTraits = [Item, Tome]
    , pcLevel = 3
    }
