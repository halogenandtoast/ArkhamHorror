module Arkham.Location.Cards.ChildrenOfBlood.NewHorizons.ManagersOfficeDay (managersOfficeDay) where

import Arkham.Ability
import Arkham.Helpers.Log (remembered)
import Arkham.Helpers.Modifiers
import Arkham.Location.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.ScenarioLogKey

newtype ManagersOfficeDay = ManagersOfficeDay LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

managersOfficeDay :: LocationCard ManagersOfficeDay
managersOfficeDay = symbolLabel $ location ManagersOfficeDay Cards.managersOfficeDay 4 (PerPlayer 1)

instance HasModifiersFor ManagersOfficeDay where
  getModifiersFor (ManagersOfficeDay a) = unless a.revealed do
    stoleKeys <- remembered TheInvestigatorsStoleTheManagersKeys
    unless stoleKeys $ modifySelect a Anyone [CannotEnter a.id]

instance HasAbilities ManagersOfficeDay where
  getAbilities (ManagersOfficeDay a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> exists (enemyAt a <> ExhaustedEnemy)) actionAbility

instance RunMessage ManagersOfficeDay where
  runMessage msg l@(ManagersOfficeDay attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      exhausted <- select $ enemyAt attrs <> ExhaustedEnemy
      chooseTargetM iid exhausted \eid -> do
        addToVictory iid eid
        doStep 1 msg
      pure l
    DoStep 1 (UseThisAbility _ (isSource attrs -> True) 1) -> do
      west <- selectJust $ LocationWithFullTitle "Factory Floor" "West"
      selectEach (enemyAt attrs) \eid -> enemyMoveTo (attrs.ability 1) eid west
      selectEach (investigatorAt attrs) \iid -> moveTo (attrs.ability 1) iid west
      removeLocation attrs
      pure l
    _ -> ManagersOfficeDay <$> liftRunMessage msg attrs
