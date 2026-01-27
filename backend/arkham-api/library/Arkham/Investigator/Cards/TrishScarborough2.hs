module Arkham.Investigator.Cards.TrishScarborough2 (trishScarborough2) where

import Arkham.Action.Additional
import Arkham.Helpers.Location
import Arkham.Helpers.Modifiers (modifySelf1)
import Arkham.I18n
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Modifier

newtype TrishScarborough2 = TrishScarborough2 InvestigatorAttrs
  deriving anyclass IsInvestigator
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

trishScarborough2 :: InvestigatorCard TrishScarborough2
trishScarborough2 =
  investigator TrishScarborough2 Cards.trishScarborough2
    $ Stats {health = 8, sanity = 6, willpower = 2, intellect = 4, combat = 2, agility = 4}

instance HasModifiersFor TrishScarborough2 where
  getModifiersFor (TrishScarborough2 a) =
    modifySelf1 a
      $ GiveAdditionalAction
      $ AdditionalAction "Trish Scarborough" (toSource a)
      $ ActionRestrictedAdditionalAction #evade

instance HasAbilities TrishScarborough2 where
  getAbilities (TrishScarborough2 _) = []

instance HasChaosTokenValue TrishScarborough2 where
  getChaosTokenValue iid ElderSign (TrishScarborough2 attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 0)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage TrishScarborough2 where
  runMessage msg i@(TrishScarborough2 attrs) = runQueueT $ case msg of
    ElderSignEffect iid | attrs `is` iid -> do
      afterSkillTest iid "Trish Scarborough" do
        chooseOneM iid do
          labeled "Disengage from each enemy and move to a connecting location." $ doStep 1 msg
          withI18n skip_
      pure i
    DoStep 1 (ElderSignEffect iid) | attrs `is` iid -> do
      selectEach (enemyEngagedWith iid) (disengageEnemy iid)
      locations <- getConnectedMoveLocations iid (attrs.ability 1)
      chooseTargetM iid locations (moveTo (attrs.ability 1) iid)
      pure i
    _ -> TrishScarborough2 <$> liftRunMessage msg attrs
