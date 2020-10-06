module Arkham.Types.Card.PlayerCard.Cards.SearchingForIzzie where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Classes.RunMessage
import Arkham.Types.Trait

newtype SearchingForIzzie = SearchingForIzzie Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance (HasQueue env) => RunMessage env SearchingForIzzie where
  runMessage msg (SearchingForIzzie attrs) =
    SearchingForIzzie <$> runMessage msg attrs

searchingForIzzie :: CardId -> SearchingForIzzie
searchingForIzzie cardId =
  SearchingForIzzie $ (treachery cardId "02011" "Searching for Izzie" 0)
    { pcTraits = [Task]
    , pcRevelation = True
    }

