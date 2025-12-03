module Arkham.Act.Cards.RabbitsWhoRunV2 (rabbitsWhoRunV2) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Keys
import Arkham.Campaigns.TheScarletKeys.Key.Matcher
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher

newtype RabbitsWhoRunV2 = RabbitsWhoRunV2 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rabbitsWhoRunV2 :: ActCard RabbitsWhoRunV2
rabbitsWhoRunV2 = act (1, A) RabbitsWhoRunV2 Cards.rabbitsWhoRunV2 Nothing

instance HasAbilities RabbitsWhoRunV2 where
  getAbilities = actAbilities \a ->
    [ mkAbility a 1
        $ Objective
        $ forced
        $ IfEnemyDefeated #after Anyone ByAny (enemyIs Enemies.theClaretKnightCoterieKingpin)
    , restricted a 1 (exists $ InvestigatorWithScarletKey (scarletKeyIs Keys.theLightOfPharos))
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage RabbitsWhoRunV2 where
  runMessage msg a@(RabbitsWhoRunV2 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R2
      pure a
    _ -> RabbitsWhoRunV2 <$> liftRunMessage msg attrs
