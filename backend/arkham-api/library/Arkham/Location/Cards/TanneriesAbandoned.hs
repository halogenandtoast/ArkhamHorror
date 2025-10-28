module Arkham.Location.Cards.TanneriesAbandoned (tanneriesAbandoned) where

import Arkham.Helpers.Modifiers (ModifierType (EnemyEvade, EnemyFight), modifySelect)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Trait (Trait (Risen))

newtype TanneriesAbandoned = TanneriesAbandoned LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

tanneriesAbandoned :: LocationCard TanneriesAbandoned
tanneriesAbandoned = symbolLabel $ location TanneriesAbandoned Cards.tanneriesAbandoned 3 (Static 0)

instance HasModifiersFor TanneriesAbandoned where
  getModifiersFor (TanneriesAbandoned a) =
    modifySelect a (enemyAt a <> EnemyWithTrait Risen) [EnemyFight 1, EnemyEvade 1]

instance RunMessage TanneriesAbandoned where
  runMessage msg (TanneriesAbandoned attrs) = TanneriesAbandoned <$> runMessage msg attrs
