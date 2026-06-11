module Arkham.Location.Cards.ChamberOfRegret (chamberOfRegret) where

import Arkham.Ability
import Arkham.Classes.HasGame
import Arkham.Helpers.Act (getCurrentActStep)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectWhen, modifySelfWhen)
import Arkham.Helpers.Scenario (scenarioFieldMap)
import Arkham.I18n
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose hiding (labeled)
import Arkham.Message.Lifted.Log
import Arkham.Name
import Arkham.Projection
import Arkham.Scenario.Types (Field (..))
import Arkham.ScenarioLogKey
import Arkham.Scenarios.TheLabyrinthsOfLunacy.Helpers
import Arkham.Tracing

newtype ChamberOfRegret = ChamberOfRegret LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chamberOfRegret :: LocationCard ChamberOfRegret
chamberOfRegret = location ChamberOfRegret Cards.chamberOfRegret 1 (PerPlayer 1)

anyLeverPulled :: (HasGame m, Tracing m) => m Bool
anyLeverPulled =
  scenarioFieldMap ScenarioRemembered $ any \case
    PulledTheLeftLever _ -> True
    PulledTheMiddleLever _ -> True
    PulledTheRightLever _ -> True
    _ -> False

-- No more than one investigator may be in the Chamber of Regret at a time.
-- Once the lever is pulled, the gate slams shut: the puller cannot leave for
-- the remainder of the current act (the levers only matter during act 1).
instance HasModifiersFor ChamberOfRegret where
  getModifiersFor (ChamberOfRegret a) = do
    occupied <- selectAny $ investigatorAt a.id
    modifySelfWhen a occupied [Blocked]
    step <- getCurrentActStep
    leverPulled <- anyLeverPulled
    modifySelectWhen a (step == 1 && leverPulled) (investigatorAt a.id) [CannotMove]

instance HasAbilities ChamberOfRegret where
  getAbilities (ChamberOfRegret a) =
    extendRevealed1 a
      $ restricted a 1 Here
      $ actionAbilityWithCost (GroupClueCost (PerPlayer 2) Anywhere)

instance RunMessage ChamberOfRegret where
  runMessage msg l@(ChamberOfRegret attrs) = runQueueT $ scenarioI18n $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      name <- field InvestigatorName iid
      chooseOneM iid $ scope "chamberOfRegret" do
        questionLabeled' "whichLever"
        labeled' "left" $ remember $ PulledTheLeftLever $ labeled name iid
        labeled' "middle" $ remember $ PulledTheMiddleLever $ labeled name iid
        labeled' "right" $ remember $ PulledTheRightLever $ labeled name iid
      pure l
    _ -> ChamberOfRegret <$> liftRunMessage msg attrs
