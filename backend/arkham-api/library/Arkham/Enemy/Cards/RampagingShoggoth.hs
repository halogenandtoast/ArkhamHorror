module Arkham.Enemy.Cards.RampagingShoggoth (rampagingShoggoth) where

import Arkham.Ability
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Shoggoth))

newtype RampagingShoggoth = RampagingShoggoth EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rampagingShoggoth :: EnemyCard RampagingShoggoth
rampagingShoggoth = enemy RampagingShoggoth Cards.rampagingShoggoth (3, Static 4, 1) (2, 1)

instance HasModifiersFor RampagingShoggoth where
  getModifiersFor (RampagingShoggoth a) = do
    healthModifier <- perPlayer 2
    modifySelf a [HealthModifier healthModifier]

instance HasAbilities RampagingShoggoth where
  getAbilities (RampagingShoggoth a) =
    extend1 a
      $ restricted
        a
        1
        ( oneOf
            [ exists $ at_ (locationWithEnemy a.id) <> not_ (EnemyWithTrait Shoggoth)
            , exists $ investigator_ $ at_ $ locationWithEnemy a.id
            ]
        )
      $ forced
      $ PhaseEnds #when #enemy

instance RunMessage RampagingShoggoth where
  runMessage msg e@(RampagingShoggoth attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      investigators <- select $ investigator_ $ at_ $ locationWithEnemy attrs
      enemies <- select $ at_ (locationWithEnemy attrs) <> not_ (EnemyWithTrait Shoggoth)
      lead <- getLead
      chooseOneAtATimeM lead do
        targets enemies $ nonAttackEnemyDamage (attrs.ability 1) 2
        targets investigators \iid -> assignDamage iid (attrs.ability 1) 2

      for_ investigators \iid -> drawTekelili iid (attrs.ability 1) 1
      pure e
    _ -> RampagingShoggoth <$> liftRunMessage msg attrs
