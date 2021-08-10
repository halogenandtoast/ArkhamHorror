module Arkham.Types.Act.Cards.SearchingForAnswers
  ( SearchingForAnswers(..)
  , searchingForAnswers
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import qualified Arkham.Enemy.Cards as Enemies
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.ActId
import Arkham.Types.Card
import Arkham.Types.Card.EncounterCard
import Arkham.Types.Classes
import Arkham.Types.Game.Helpers
import Arkham.Types.LocationId
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Target

newtype SearchingForAnswers = SearchingForAnswers ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasActions, HasModifiersFor env)

searchingForAnswers :: ActCard SearchingForAnswers
searchingForAnswers =
  act (1, A) SearchingForAnswers Cards.searchingForAnswers Nothing

instance ActRunner env => RunMessage env SearchingForAnswers where
  runMessage msg a@(SearchingForAnswers attrs@ActAttrs {..}) = case msg of
    WhenEnterLocation _ lid -> do
      mHiddenChamberId <- getLocationIdByName "The Hidden Chamber"
      a <$ when
        (Just lid == mHiddenChamberId)
        (push $ AdvanceAct actId (toSource attrs))
    AdvanceAct aid _ | aid == actId && onSide A attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      push $ chooseOne leadInvestigatorId [AdvanceAct aid (toSource attrs)]
      pure
        . SearchingForAnswers
        $ attrs
        & (sequenceL .~ Act (unActStep $ actStep actSequence) B)
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
      unrevealedLocationIds <- map unUnrevealedLocationId <$> getSetList ()
      hiddenChamber <- fromJustNote "must exist"
        <$> getId (LocationWithTitle "The Hidden Chamber")
      silasBishop <- EncounterCard <$> genEncounterCard Enemies.silasBishop
      a <$ pushAll
        ([ RevealLocation Nothing lid | lid <- unrevealedLocationIds ]
        <> [ MoveAllCluesTo (LocationTarget hiddenChamber)
           , CreateEnemyAt silasBishop hiddenChamber Nothing
           , NextAct aid "02200"
           ]
        )
    _ -> SearchingForAnswers <$> runMessage msg attrs
