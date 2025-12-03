module Arkham.Act.Cards.RabbitsWhoRunV3 (rabbitsWhoRunV3) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.TheScarletKeys.Key
import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Keys
import Arkham.Campaigns.TheScarletKeys.Key.Matcher
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher

newtype RabbitsWhoRunV3 = RabbitsWhoRunV3 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rabbitsWhoRunV3 :: ActCard RabbitsWhoRunV3
rabbitsWhoRunV3 = act (1, A) RabbitsWhoRunV3 Cards.rabbitsWhoRunV3 Nothing

instance HasAbilities RabbitsWhoRunV3 where
  getAbilities = actAbilities \a ->
    [ mkAbility a 1
        $ Objective
        $ forced
        $ IfEnemyDefeated #after Anyone ByAny (enemyIs Enemies.theBeastInACowlOfCrimsonWolfInSheepsClothing)
    , restricted a 1 (exists $ InvestigatorWithScarletKey (scarletKeyIs Keys.theLightOfPharos))
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage RabbitsWhoRunV3 where
  runMessage msg a@(RabbitsWhoRunV3 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      madeADealWithTheClaretKnight <- getHasRecord TheCellMadeADealWithTheClaretKnight
      push $ if madeADealWithTheClaretKnight then R5 else R6
      pure a
    _ -> RabbitsWhoRunV3 <$> liftRunMessage msg attrs
