module Arkham.Enemy.Cards.TheBloodlessMan (theBloodlessMan) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyDamage, EnemyDefeated)
import Arkham.Enemy.Types (Field (EnemyDamage))
import Arkham.Helpers.GameValue
import Arkham.Helpers.Location
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Projection
import Arkham.Trait

newtype TheBloodlessMan = TheBloodlessMan EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBloodlessMan :: EnemyCard TheBloodlessMan
theBloodlessMan = enemy TheBloodlessMan Cards.theBloodlessMan (4, Static 3, 2) (1, 1)

instance HasModifiersFor TheBloodlessMan where
  getModifiersFor (TheBloodlessMan a) =
    modifySelf
      a
      [ AddKeyword Keyword.Aloof
      , AddKeyword $ Keyword.Patrol (NearestLocationToYou $ LocationWithAsset $ AssetWithTrait Guest)
      ]

instance HasAbilities TheBloodlessMan where
  getAbilities (TheBloodlessMan a) =
    extend
      a
      [ mkAbility a 1 $ forced $ EnemyDefeated #when Anyone ByAny (be a)
      , restricted a 2 (thisExists a ReadyEnemy) $ forced $ PhaseEnds #when #investigation
      ]

instance RunMessage TheBloodlessMan where
  runMessage msg e@(TheBloodlessMan attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      n <- perPlayer 3
      damage <- field EnemyDamage attrs.id
      let healAmt = max 0 (damage - n)
      healDamage attrs.id (attrs.ability 1) healAmt
      push $ Exhaust (toTarget attrs)
      flipOverBy iid (attrs.ability 1) attrs
      pure e
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      withLocationOf attrs \lid -> do
        selectEach (investigatorAt lid) \i -> assignHorror i (attrs.ability 2) 1
        selectEach (AssetWithTrait Guest <> assetAt lid) \aid -> dealAssetHorror aid (attrs.ability 2) 1
      pure e
    _ -> TheBloodlessMan <$> liftRunMessage msg attrs
