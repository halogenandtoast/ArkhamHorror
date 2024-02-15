module Arkham.Act.Cards.SearchingForAnswers (
  SearchingForAnswers (..),
  searchingForAnswers,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Timing qualified as Timing

newtype SearchingForAnswers = SearchingForAnswers ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

searchingForAnswers :: ActCard SearchingForAnswers
searchingForAnswers =
  act (1, A) SearchingForAnswers Cards.searchingForAnswers Nothing

instance HasAbilities SearchingForAnswers where
  getAbilities (SearchingForAnswers x) =
    [ mkAbility x 1
        $ ForcedAbility
        $ Enters Timing.When You
        $ LocationWithTitle
          "The Hidden Chamber"
    ]

instance RunMessage SearchingForAnswers where
  runMessage msg a@(SearchingForAnswers attrs) = case msg of
    UseCardAbility _ source 1 _ _
      | isSource attrs source ->
          a <$ push (AdvanceAct (toId attrs) source AdvancedWithOther)
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      unrevealedLocationIds <- select UnrevealedLocation
      hiddenChamber <- selectJust (LocationWithTitle "The Hidden Chamber")
      silasBishop <- genCard Enemies.silasBishop
      createSilasBishop <- createEnemyAt_ silasBishop hiddenChamber Nothing
      pushAll
        $ [RevealLocation Nothing lid | lid <- unrevealedLocationIds]
        <> [ MoveAllCluesTo (toSource attrs) (toTarget hiddenChamber)
           , createSilasBishop
           , AdvanceActDeck (actDeckId attrs) (toSource attrs)
           ]
      pure a
    _ -> SearchingForAnswers <$> runMessage msg attrs
