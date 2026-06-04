module Arkham.Location.Cards.TheOldMillNight (theOldMillNight) where

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

newtype TheOldMillNight = TheOldMillNight LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theOldMillNight :: LocationCard TheOldMillNight
theOldMillNight = symbolLabel $ location TheOldMillNight Cards.theOldMillNight 0 (Static 0)

whenFateOfTheValeV4 :: ReverseQueue m => m () -> m ()
whenFateOfTheValeV4 body = do
  act <- getCurrentAct
  actCard <- field ActCard act
  when (toCardCode actCard == toCardCode Acts.fateOfTheValeV4) body

oldMillReward :: ReverseQueue m => TheOldMillNight -> InvestigatorId -> m TheOldMillNight
oldMillReward l@(TheOldMillNight attrs) iid = do
  healDamage iid (attrs.ability 1) 2
  whenFateOfTheValeV4 $ remember TheSamplesWereFound
  pure l

instance HasAbilities TheOldMillNight where
  getAbilities (TheOldMillNight a) =
    extendRevealed1 a $ restricted a 1 Here actionAbility

instance RunMessage TheOldMillNight where
  runMessage msg l@(TheOldMillNight attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed 8)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> oldMillReward l iid
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      chooseOneM iid do
        labeled "Spend clues to automatically succeed" do
          withCost iid (GroupClueCost (PerPlayer 2) Anywhere) $ doStep 1 msg
        labeled "Do not spend clues" nothing
      pure l
    DoStep 1 (FailedThisSkillTest iid (isAbilitySource attrs 1 -> True)) -> oldMillReward l iid
    _ -> TheOldMillNight <$> liftRunMessage msg attrs
