module Arkham.Enemy.Cards.TakadaHirokoAeroplaneMechanic (takadaHirokoAeroplaneMechanic) where

import Arkham.Ability
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyDefeated)
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Matcher

newtype TakadaHirokoAeroplaneMechanic = TakadaHirokoAeroplaneMechanic EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

takadaHirokoAeroplaneMechanic :: EnemyCard TakadaHirokoAeroplaneMechanic
takadaHirokoAeroplaneMechanic = enemy TakadaHirokoAeroplaneMechanic Cards.takadaHirokoAeroplaneMechanic (3, Static 3, 3) (2, 0)

instance HasAbilities TakadaHirokoAeroplaneMechanic where
  getAbilities (TakadaHirokoAeroplaneMechanic a) =
    extend
      a
      [ restricted a 1 OnSameLocation $ parleyAction (UpTo (Fixed 3) $ ResourceCost 1)
      , mkAbility a 2 $ forced $ EnemyDefeated #when Anyone ByAny (be a)
      ]

instance RunMessage TakadaHirokoAeroplaneMechanic where
  runMessage msg e@(TakadaHirokoAeroplaneMechanic attrs) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 _ (totalResourcePayment -> n) -> do
      placeTokens (attrs.ability 1) attrs #resource n
      doStep 2 msg
      pure e
    DoStep 2 (UseThisAbility _iid (isSource attrs -> True) 1) -> do
      n <- perPlayer 3
      when (attrs.token #resource >= n) $ addToVictory attrs
      pure e
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      eliminatePartner attrs
      pure e
    _ -> TakadaHirokoAeroplaneMechanic <$> liftRunMessage msg attrs
