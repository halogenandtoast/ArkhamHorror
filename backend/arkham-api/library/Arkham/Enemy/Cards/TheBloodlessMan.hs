module Arkham.Enemy.Cards.TheBloodlessMan (theBloodlessMan, TheBloodlessMan(..)) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Trait

newtype TheBloodlessMan = TheBloodlessMan EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBloodlessMan :: EnemyCard TheBloodlessMan
theBloodlessMan =
  unique $ enemy TheBloodlessMan Cards.theBloodlessMan (4, Static 3, 2) (1, 1)

instance HasModifiersFor TheBloodlessMan where
  getModifiersFor (TheBloodlessMan a) =
    modifySelf a
      [ AddKeyword Keyword.Aloof
      , AddKeyword $ Keyword.Patrol (NearestLocationToYou $ LocationWithAsset $ AssetWithTrait Guest)
      ]

instance HasAbilities TheBloodlessMan where
  getAbilities (TheBloodlessMan a) = withBaseAbilities a [
      mkAbility a 1 $ forced $ EnemyDefeated #when Anyone ByAny (be a)
    , restrictedAbility a 2 (thisExists a ReadyEnemy) $ forced $ PhaseEnds #when #investigation
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
      mLoc <- field EnemyLocation attrs.id
      for_ mLoc \lid -> do
        investigators <- select $ investigatorAt lid
        guests <- select $ AssetWithTrait Guest <> assetAt lid
        for_ investigators \i -> assignHorror i (attrs.ability 2) 1
        for_ guests \aid -> assignHorror aid (attrs.ability 2) 1
      pure e
    _ -> TheBloodlessMan <$> liftRunMessage msg attrs

