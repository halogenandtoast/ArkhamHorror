module Arkham.Location.Cards.BoardingHouseNight (boardingHouseNight) where

import Arkham.Ability
import Arkham.Card
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers
import Arkham.History (History (historyEnemiesDrawn, historyTreacheriesDrawn))
import Arkham.History.Types
import Arkham.Keyword qualified as Keyword
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Log (remember)
import Arkham.ScenarioLogKey
import Arkham.Scenarios.FateOfTheVale.Helpers

newtype BoardingHouseNight = BoardingHouseNight LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

boardingHouseNight :: LocationCard BoardingHouseNight
boardingHouseNight = symbolLabel $ location BoardingHouseNight Cards.boardingHouseNight 3 (PerPlayer 1)

instance HasModifiersFor BoardingHouseNight where
  getModifiersFor (BoardingHouseNight a) = do
    history <- fmap fold . traverse (getHistory PhaseHistory) =<< select (investigatorAt a)
    when (length (historyTreacheriesDrawn history) + length (historyEnemiesDrawn history) == 1) do
      cards <- findAllCards (`cardMatch` CardWithType TreacheryType)
      modifyEach a (map (CardIdTarget . toCardId) cards) [AddKeyword Keyword.Surge]

instance HasAbilities BoardingHouseNight where
  getAbilities (BoardingHouseNight a) =
    extendRevealed1 a $ restricted a 1 Here actionAbility

instance RunMessage BoardingHouseNight where
  runMessage msg l@(BoardingHouseNight attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (IndexedSource 1 $ attrs.ability 1) iid #willpower (Fixed 3)
      pure l
    PassedThisSkillTest iid (IndexedSource 1 (isAbilitySource attrs 1 -> True)) -> do
      sid <- getRandom
      beginSkillTest sid iid (IndexedSource 2 $ attrs.ability 1) iid #agility (Fixed 2)
      pure l
    PassedThisSkillTest iid (IndexedSource 2 (isAbilitySource attrs 1 -> True)) -> do
      sid <- getRandom
      beginSkillTest sid iid (IndexedSource 3 $ attrs.ability 1) iid #combat (Fixed 1)
      pure l
    PassedThisSkillTest iid (IndexedSource 3 (isAbilitySource attrs 1 -> True)) -> do
      drawCardsAndThen iid (attrs.ability 1) 3 $ whenFateOfTheValeV4 $ remember TheInvestigatorsFoundGas
      pure l
    _ -> BoardingHouseNight <$> liftRunMessage msg attrs
