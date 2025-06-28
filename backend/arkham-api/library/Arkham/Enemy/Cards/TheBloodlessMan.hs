module Arkham.Enemy.Cards.TheBloodlessMan (theBloodlessMan) where

import Arkham.Ability
import Arkham.Card
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyDamage, EnemyDefeated)
import Arkham.Enemy.Types (Field (EnemyDamage))
import Arkham.Helpers.GameValue
import Arkham.Helpers.Location
import Arkham.Matcher
import Arkham.Message (ReplaceStrategy (Swap))
import Arkham.Projection
import Arkham.Scenarios.TheMidwinterGala.Helpers
import Arkham.Trait

newtype TheBloodlessMan = TheBloodlessMan EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBloodlessMan :: EnemyCard TheBloodlessMan
theBloodlessMan = enemy TheBloodlessMan Cards.theBloodlessMan (4, PerPlayer 3, 2) (1, 1)

instance HasAbilities TheBloodlessMan where
  getAbilities (TheBloodlessMan a) =
    extend
      a
      [ mkAbility a 1 $ forced $ EnemyDefeated #when Anyone ByAny (be a)
      , restricted
          a
          2
          ( thisExists a ReadyEnemy
              <> oneOf
                [ exists (AssetAt (locationWithEnemy a) <> AssetWithTrait Guest)
                , exists (InvestigatorAt (locationWithEnemy a))
                ]
          )
          $ forced
          $ PhaseEnds #when #investigation
      ]

instance RunMessage TheBloodlessMan where
  runMessage msg e@(TheBloodlessMan attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      cancelEnemyDefeat attrs.id
      n <- perPlayer 3
      damage <- field EnemyDamage attrs.id
      let healAmt = max 0 (damage - n)
      when (healAmt > 0) $ healDamage attrs.id (attrs.ability 1) healAmt
      exhaustThis attrs
      flipOverBy iid (attrs.ability 1) attrs
      pure e
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      withLocationOf attrs \lid -> do
        selectEach (investigatorAt lid) \i -> assignHorror i (attrs.ability 2) 1
        selectEach (AssetWithTrait Guest <> assetAt lid) \aid -> dealAssetHorror aid (attrs.ability 2) 1
      doStep 2 msg
      pure e
    DoStep 2 (UseThisAbility _ (isSource attrs -> True) 2) -> do
      selectEach (AssetWithTrait Guest <> AssetAt (locationWithEnemy attrs)) becomeSpellbound
      pure e
    Flip _ _ (isTarget attrs -> True) -> do
      unleashed <- genCard Cards.theBloodlessManUnleashed
      push $ ReplaceEnemy attrs.id unleashed Swap
      pure e
    _ -> TheBloodlessMan <$> liftRunMessage msg attrs
