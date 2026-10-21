module Arkham.Enemy.Cards.TracesToNowhere.Bruiser (bruiser) where

import Arkham.Enemy.CardDefs.TracesToNowhere qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher.Asset
import Arkham.Strategy
import Arkham.Trait (Trait (Ally))

newtype Bruiser = Bruiser EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

-- | "Damage dealt by Bruiser must be assigned to Ally assets, if able."
bruiser :: EnemyCard Bruiser
bruiser =
  enemyWith Bruiser Cards.bruiser (damageStrategyL .~ DamageAssetsFirst (AssetWithTrait Ally))

instance RunMessage Bruiser where
  runMessage msg (Bruiser attrs) = Bruiser <$> runMessage msg attrs
