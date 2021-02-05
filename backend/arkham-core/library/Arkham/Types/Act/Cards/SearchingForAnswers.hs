module Arkham.Types.Act.Cards.SearchingForAnswers
  ( SearchingForAnswers(..)
  , searchingForAnswers
  ) where

import Arkham.Import

import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner

newtype SearchingForAnswers = SearchingForAnswers ActAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

searchingForAnswers :: SearchingForAnswers
searchingForAnswers = SearchingForAnswers
  $ baseAttrs "02199" "Searching for Answers" (Act 1 A) Nothing

instance ActionRunner env => HasActions env SearchingForAnswers where
  getActions iid window (SearchingForAnswers attrs) =
    getActions iid window attrs

instance ActRunner env => RunMessage env SearchingForAnswers where
  runMessage msg (SearchingForAnswers attrs) =
    SearchingForAnswers <$> runMessage msg attrs
