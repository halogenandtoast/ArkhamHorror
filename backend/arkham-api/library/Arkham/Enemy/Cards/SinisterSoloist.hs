module Arkham.Enemy.Cards.SinisterSoloist (sinisterSoloist) where

import Arkham.Ability
import Arkham.ClassSymbol
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectMap)
import Arkham.Matcher

newtype SinisterSoloist = SinisterSoloist EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sinisterSoloist :: EnemyCard SinisterSoloist
sinisterSoloist = enemy SinisterSoloist Cards.sinisterSoloist (4, PerPlayer 6, 5) (2, 2)

instance HasModifiersFor SinisterSoloist where
  getModifiersFor (SinisterSoloist a) = do
    modifySelectMap a (at_ $ locationWithEnemy a) \iid ->
      [AdditionalPlayCostOf (basic $ not_ (CardWithClass Neutral)) (HorrorCost (toSource a) (toTarget iid) 1)]

instance HasAbilities SinisterSoloist where
  getAbilities (SinisterSoloist a) =
    extend1 a
      $ restricted a 1 (exists $ EnemyWithId a.id <> ReadyEnemy)
      $ forced
      $ PhaseEnds #when #enemy

instance RunMessage SinisterSoloist where
  runMessage msg e@(SinisterSoloist attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectEach (at_ (orConnected_ $ locationWithEnemy attrs)) \iid ->
        directHorror iid (attrs.ability 1) 1
      pure e
    _ -> SinisterSoloist <$> liftRunMessage msg attrs
