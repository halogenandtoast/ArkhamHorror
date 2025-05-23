module Arkham.Enemy.Cards.Umordhoth (umordhoth) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Matcher

newtype Umordhoth = Umordhoth EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

umordhoth :: EnemyCard Umordhoth
umordhoth = enemy Umordhoth Cards.umordhoth (5, Static 6, 6) (3, 3)

instance HasModifiersFor Umordhoth where
  getModifiersFor (Umordhoth a) = do
    healthModifier <- getPlayerCountValue (PerPlayer 4)
    modifySelf a [HealthModifier healthModifier]

instance HasAbilities Umordhoth where
  getAbilities (Umordhoth a) =
    extend
      a
      [ mkAbility a 1 $ forced $ TurnEnds #after Anyone
      , restricted
          a
          2
          (OnSameLocation <> exists (AssetControlledBy You <> assetIs Cards.litaChantler))
          actionAbility
      ]

instance RunMessage Umordhoth where
  runMessage msg e@(Umordhoth attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      readyThis attrs
      pure e
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      push R3
      pure e
    _ -> Umordhoth <$> liftRunMessage msg attrs
