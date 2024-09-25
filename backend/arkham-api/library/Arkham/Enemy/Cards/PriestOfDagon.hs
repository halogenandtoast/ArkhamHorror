module Arkham.Enemy.Cards.PriestOfDagon (priestOfDagon, PriestOfDagon (..)) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyDefeated, EnemyEvaded)
import Arkham.Matcher

newtype PriestOfDagon = PriestOfDagon EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

priestOfDagon :: EnemyCard PriestOfDagon
priestOfDagon = enemyWith PriestOfDagon Cards.priestOfDagon (3, Static 2, 3) (1, 1) spawnAtEmptyLocation

instance HasAbilities PriestOfDagon where
  getAbilities (PriestOfDagon a) =
    extend
      a
      [ restrictedAbility a 1 (thisIs a $ EnemyWithoutDoom)
          $ forced
          $ oneOf [EnemyDefeated #when Anyone ByAny (be a), EnemyEvaded #when Anyone (be a)]
      , restrictedAbility a 2 (thisIs a $ EnemyWithoutDoom <> #ready) $ forced $ RoundEnds #when
      ]

instance RunMessage PriestOfDagon where
  runMessage msg e@(PriestOfDagon attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      let
        response = do
          healAllDamage (attrs.ability 1) attrs
          ready attrs
          placeDoom (attrs.ability 2) attrs 1
      insteadOfDefeat attrs response
      insteadOfEvading attrs response
      pure e
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      placeDoom (attrs.ability 2) attrs 1
      pure e
    _ -> PriestOfDagon <$> liftRunMessage msg attrs
