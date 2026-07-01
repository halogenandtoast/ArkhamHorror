module Arkham.Enemy.Cards.DroningHorde (droningHorde) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Investigator.Projection ()
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.WarOfTheOuterGods.Helpers
import Arkham.Trait (Trait (Insect))

newtype DroningHorde = DroningHorde EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

droningHorde :: EnemyCard DroningHorde
droningHorde = enemy DroningHorde Cards.droningHorde

instance HasModifiersFor DroningHorde where
  getModifiersFor (DroningHorde a) = modifySelf a [AttackDealsEitherDamageOrHorror]

instance HasAbilities DroningHorde where
  getAbilities (DroningHorde a) =
    extend1 a $ forcedAbility a 1 $ EnemyEntersPlay #after (be a <> not_ IsSwarm)

instance RunMessage DroningHorde where
  runMessage msg e@(DroningHorde attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      eachInvestigator (`forTarget` msg)
      pure e
    ForTarget (InvestigatorTarget iid) (UseThisAbility _ (isSource attrs -> True) 1) -> do
      hand <- iid.hand
      insects <- select $ EnemyWithTrait Insect <> not_ IsSwarm
      when (notNull insects && notNull hand) do
        chooseOneM iid $ targets hand \card -> do
          chooseOrRunOneM iid $ targets insects (`placeCardAsSwarm` card)
      pure e
    _ -> DroningHorde <$> liftRunMessage msg attrs
