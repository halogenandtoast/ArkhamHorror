module Arkham.Types.Act.Cards.SearchingForAnswers
  ( SearchingForAnswers(..)
  , searchingForAnswers
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import qualified Arkham.Enemy.Cards as Enemies
import Arkham.Types.Ability
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.Card
import Arkham.Types.Card.EncounterCard
import Arkham.Types.Classes
import Arkham.Types.Matcher hiding (RevealLocation)
import Arkham.Types.Message
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing

newtype SearchingForAnswers = SearchingForAnswers ActAttrs
  deriving anyclass (IsAct, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

searchingForAnswers :: ActCard SearchingForAnswers
searchingForAnswers =
  act (1, A) SearchingForAnswers Cards.searchingForAnswers Nothing

instance HasAbilities SearchingForAnswers where
  getAbilities (SearchingForAnswers x) =
    [ mkAbility x 1 $ ForcedAbility $ Enters Timing.When You $ LocationWithTitle
        "The Hidden Chamber"
    ]

instance ActRunner env => RunMessage env SearchingForAnswers where
  runMessage msg a@(SearchingForAnswers attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      a <$ push (AdvanceAct (toId attrs) source)
    AdvanceAct aid _ | aid == toId attrs && onSide B attrs -> do
      unrevealedLocationIds <- selectList UnrevealedLocation
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
