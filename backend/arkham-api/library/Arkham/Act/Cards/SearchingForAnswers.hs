module Arkham.Act.Cards.SearchingForAnswers ( searchingForAnswers,) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher hiding (RevealLocation)

newtype SearchingForAnswers = SearchingForAnswers ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

searchingForAnswers :: ActCard SearchingForAnswers
searchingForAnswers = act (1, A) SearchingForAnswers Cards.searchingForAnswers Nothing

instance HasAbilities SearchingForAnswers where
  getAbilities (SearchingForAnswers x) =
    [mkAbility x 1 $ forced $ Enters #when You "The Hidden Chamber"]

instance RunMessage SearchingForAnswers where
  runMessage msg a@(SearchingForAnswers attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      selectEach UnrevealedLocation reveal

      hiddenChamber <- selectJust (LocationWithTitle "The Hidden Chamber")
      push $ MoveAllCluesTo (toSource attrs) (toTarget hiddenChamber)

      createEnemyAt_ Enemies.silasBishop hiddenChamber
      advanceActDeck attrs
      pure a
    _ -> SearchingForAnswers <$> liftRunMessage msg attrs
