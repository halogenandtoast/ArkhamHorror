module Arkham.Treachery.Cards.LightOutOfVoid (lightOutOfVoid) where

import Arkham.Investigator.Types (Field (..))
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.Act
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.DealingsInTheDark.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype LightOutOfVoid = LightOutOfVoid TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lightOutOfVoid :: TreacheryCard LightOutOfVoid
lightOutOfVoid = treachery LightOutOfVoid Cards.lightOutOfVoid

instance RunMessage LightOutOfVoid where
  runMessage msg t@(LightOutOfVoid attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      doStep 1 msg
      pure t
    DoStep n msg'@(Revelation iid (isSource attrs -> True)) -> do
      cultists <- selectMaxBy EnemyEvade (fromMaybe (-1)) (InPlayEnemy #cultist)
      chooseOneM iid $ scenarioI18n do
        labeledValidate' (notNull cultists) "lightOutOfVoid.doom" do
          chooseTargetM iid cultists $ placeDoomOn attrs 1
        unscoped
          $ numberVar "damage" 1
          $ numberVar "horror" 1
          $ labeled' "takeDamageAndHorror"
          $ assignDamageAndHorror iid (attrs.ability 1) 1 1
      when (n == 1) do
        act <- getCurrentActStep
        cultistClues <- getCluesPossesedByTheCult
        playerClues <- selectSum InvestigatorClues Anyone
        when (act == 3 || playerClues > cultistClues) $ doStep 2 msg'
      pure t
    _ -> LightOutOfVoid <$> liftRunMessage msg attrs
