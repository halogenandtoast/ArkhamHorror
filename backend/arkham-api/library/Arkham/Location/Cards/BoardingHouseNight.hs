module Arkham.Location.Cards.BoardingHouseNight (boardingHouseNight) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Types (Field (ActCard))
import Arkham.Card (toCardCode, toCardId)
import Arkham.Helpers.Act (getCurrentAct)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Log (remember)
import Arkham.Projection
import Arkham.ScenarioLogKey
import Arkham.Treachery.Import.Lifted (gainSurge)

newtype BoardingHouseNight = BoardingHouseNight LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

boardingHouseNight :: LocationCard BoardingHouseNight
boardingHouseNight = symbolLabel $ location BoardingHouseNight Cards.boardingHouseNight 0 (Static 0)

whenFateOfTheValeV4 :: ReverseQueue m => m () -> m ()
whenFateOfTheValeV4 body = do
  act <- getCurrentAct
  actCard <- field ActCard act
  when (toCardCode actCard == toCardCode Acts.fateOfTheValeV4) body

instance HasAbilities BoardingHouseNight where
  getAbilities (BoardingHouseNight a) =
    extendRevealed1 a $ restricted a 1 Here actionAbility

instance RunMessage BoardingHouseNight where
  runMessage msg l@(BoardingHouseNight attrs) = runQueueT $ case msg of
    BeginRound -> pure $ BoardingHouseNight $ attrs & setMeta True
    InvestigatorDrewEncounterCard iid ec | getLocationMetaDefault True attrs -> do
      atBoardingHouse <- iid <=~> InvestigatorAt (be attrs)
      if atBoardingHouse
        then do
          push $ GainSurge (attrs.ability 1) (CardIdTarget $ toCardId ec)
          pure $ BoardingHouseNight $ attrs & setMeta False
        else pure l
    Revelation iid (TreacherySource tid) | getLocationMetaDefault True attrs -> do
      atBoardingHouse <- iid <=~> InvestigatorAt (be attrs)
      if atBoardingHouse
        then do
          priority $ gainSurge tid
          pure $ BoardingHouseNight $ attrs & setMeta False
        else pure l
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
      drawCards iid (attrs.ability 1) 3
      whenFateOfTheValeV4 $ remember TheInvestigatorsFoundGas
      pure l
    _ -> BoardingHouseNight <$> liftRunMessage msg attrs
