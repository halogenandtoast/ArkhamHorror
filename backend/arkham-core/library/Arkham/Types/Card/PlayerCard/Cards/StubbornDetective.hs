module Arkham.Types.Card.PlayerCard.Cards.StubbornDetective where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import qualified Arkham.Types.Keyword as Keyword
import Arkham.Types.Trait

newtype StubbornDetective = StubbornDetective Attrs
  deriving newtype (Show, ToJSON, FromJSON)

stubbornDetective :: CardId -> StubbornDetective
stubbornDetective cardId =
  StubbornDetective $ (enemy cardId "01103" "Stubborn Detective" 0)
    { pcTraits = [Humanoid, Detective]
    , pcKeywords = [Keyword.Hunter]
    }
