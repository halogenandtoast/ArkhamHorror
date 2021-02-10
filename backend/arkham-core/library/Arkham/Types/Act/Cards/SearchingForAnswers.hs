module Arkham.Types.Act.Cards.SearchingForAnswers
  ( SearchingForAnswers(..)
  , searchingForAnswers
  ) where

import Arkham.Prelude

import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.Message
import Arkham.Types.Target
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
  runMessage msg a@(SearchingForAnswers attrs@ActAttrs {..}) = case msg of
    WhenEnterLocation _ "02214" ->
      a <$ unshiftMessage (AdvanceAct actId (toSource attrs))
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
      unrevealedLocationIds <- map unUnrevealedLocationId <$> getSetList ()
      hiddenChamber <- fromJustNote "must exist"
        <$> getId (LocationWithTitle "The Hidden Chamber")
      silasBishop <- EncounterCard <$> genEncounterCard "02216"
      a <$ unshiftMessages
        ([ RevealLocation Nothing lid | lid <- unrevealedLocationIds ]
        <> [ MoveAllCluesTo (LocationTarget hiddenChamber)
           , CreateEnemyAt silasBishop hiddenChamber
           , NextAct aid "02200"
           ]
        )
    _ -> SearchingForAnswers <$> runMessage msg attrs
