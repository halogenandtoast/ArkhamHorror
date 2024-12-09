module Arkham.Location.Cards.ShoresOfRlyeh (shoresOfRlyeh, ShoresOfRlyeh (..)) where

import Arkham.Ability
import Arkham.Asset.Types (Field (..))
import Arkham.Enemy.Types (Field (..))
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Treachery.Types (Field (..))

newtype ShoresOfRlyeh = ShoresOfRlyeh LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shoresOfRlyeh :: LocationCard ShoresOfRlyeh
shoresOfRlyeh = location ShoresOfRlyeh Cards.shoresOfRlyeh 1 (Static 2)

instance HasModifiersFor ShoresOfRlyeh where
  getModifiersFor (ShoresOfRlyeh a) = do
    enemyDoom <- selectAgg Sum EnemyDoom $ enemyAt a
    treacheryDoom <- selectAgg Sum TreacheryDoom $ treacheryAt a
    assetDoom <- selectAgg Sum AssetDoom $ assetAt a
    investigatorDoom <- selectAgg Sum InvestigatorDoom $ investigatorAt a
    doomOnSelf <- fieldMap LocationDoom Sum a.id
    modifySelf
      a
      [ ShroudModifier $ getSum $ fold [enemyDoom, treacheryDoom, assetDoom, investigatorDoom, doomOnSelf]
      ]

instance HasAbilities ShoresOfRlyeh where
  getAbilities (ShoresOfRlyeh a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ PutLocationIntoPlay #after Anyone (be a)

instance RunMessage ShoresOfRlyeh where
  runMessage msg l@(ShoresOfRlyeh attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeDoom (attrs.ability 1) attrs 1
      pure l
    _ -> ShoresOfRlyeh <$> liftRunMessage msg attrs
