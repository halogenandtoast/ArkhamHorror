module Arkham.Enemy.Cards.GangInformant (gangInformant) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (DiscoverClues)
import Arkham.Helpers.Cost
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype GangInformant = GangInformant EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gangInformant :: EnemyCard GangInformant
gangInformant =
  enemy GangInformant Cards.gangInformant
    & setSpawnAt (NearestLocationToYou LocationWithAnyClues)

instance HasAbilities GangInformant where
  getAbilities (GangInformant a) =
    extend1 a $ mkAbility a 1 $ forced $ DiscoverClues #after You (locationWithEnemy a) (atLeast 1)

instance RunMessage GangInformant where
  runMessage msg e@(GangInformant attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      n <- getSpendableResources iid
      chooseOneM iid $ withI18n do
        countVar 3 $ labeledValidate' (n > 0) "spendResources" $ spendResources iid 3
        countVar 1 $ labeled' "placeDoomOnAgenda" $ placeDoomOnAgenda 1
      pure e
    _ -> GangInformant <$> liftRunMessage msg attrs
