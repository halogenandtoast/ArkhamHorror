module Arkham.Enemy.Cards.AtlachNacha (
  atlachNacha,
  AtlachNacha (..),
)
where

import Arkham.Card
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Placement
import Data.Aeson (Result (..))

newtype Meta = Meta {rotation :: Int}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype AtlachNacha = AtlachNacha EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

atlachNacha :: EnemyCard AtlachNacha
atlachNacha =
  enemyWith
    AtlachNacha
    Cards.atlachNacha
    (0, Static 1, 0)
    (0, 0)
    ((asSelfLocationL ?~ "atlachNacha") . setMeta @Meta (Meta 0))

instance HasModifiersFor AtlachNacha where
  getModifiersFor target (AtlachNacha attrs) | attrs `is` target = do
    pure $ toModifiers attrs [Omnipotent | attrs.placement == Global]
  getModifiersFor _ _ = pure []

instance RunMessage AtlachNacha where
  runMessage msg e@(AtlachNacha attrs) = runQueueT $ case msg of
    Flip iid _ (isTarget attrs -> True) -> do
      lids <- liftA2 (<|>) (selectMax LocationDoom Anywhere) (select Anywhere)
      push $ Flipped (toSource attrs) (toCard attrs)
      chooseOrRunOne
        iid
        [targetLabel lid [PlaceEnemy attrs.id $ AtLocation lid] | lid <- lids]
      pure $ AtlachNacha $ attrs & asSelfLocationL .~ Nothing & flippedL .~ True
    HandleAbilityOption _ (isSource attrs -> True) n -> do
      let
        Meta m =
          case fromJSON @Meta attrs.meta of
            Success a -> a
            Error _ -> Meta 0
      pure $ AtlachNacha $ setMeta @Meta (Meta ((m + (45 * n)) `mod` 360)) attrs
    _ -> AtlachNacha <$> lift (runMessage msg attrs)
