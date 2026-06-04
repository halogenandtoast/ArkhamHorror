module Arkham.Location.Cards.TheAtwoodHouseNight (theAtwoodHouseNight) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Types (Field (ActCard))
import Arkham.Card (toCardCode)
import Arkham.Helpers.Act (getCurrentAct)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log (remember)
import Arkham.Projection
import Arkham.ScenarioLogKey

newtype TheAtwoodHouseNight = TheAtwoodHouseNight LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theAtwoodHouseNight :: LocationCard TheAtwoodHouseNight
theAtwoodHouseNight = symbolLabel $ location TheAtwoodHouseNight Cards.theAtwoodHouseNight 0 (Static 0)

whenFateOfTheValeV4 :: ReverseQueue m => m () -> m ()
whenFateOfTheValeV4 body = do
  act <- getCurrentAct
  actCard <- field ActCard act
  when (toCardCode actCard == toCardCode Acts.fateOfTheValeV4) body

atwoodReward :: ReverseQueue m => TheAtwoodHouseNight -> InvestigatorId -> m TheAtwoodHouseNight
atwoodReward l@(TheAtwoodHouseNight attrs) iid = do
  healHorror iid (attrs.ability 2) 2
  whenFateOfTheValeV4 $ remember TheSurveyNotesWereRecovered
  pure l

instance HasAbilities TheAtwoodHouseNight where
  getAbilities (TheAtwoodHouseNight a) =
    extendRevealed
      a
      [ restricted a 1 Here $ forced $ SkillTestResult #after You (WhileInvestigating $ be a) #failure
      , restricted a 2 Here actionAbility
      ]

instance RunMessage TheAtwoodHouseNight where
  runMessage msg l@(TheAtwoodHouseNight attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid (attrs.ability 1) 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 2) iid #willpower (Fixed 8)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> atwoodReward l iid
    FailedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      chooseOneM iid do
        labeled "Spend clues to automatically succeed" do
          withCost iid (GroupClueCost (PerPlayer 2) Anywhere) $ doStep 1 msg
        labeled "Do not spend clues" nothing
      pure l
    DoStep 1 (FailedThisSkillTest iid (isAbilitySource attrs 2 -> True)) -> atwoodReward l iid
    _ -> TheAtwoodHouseNight <$> liftRunMessage msg attrs
