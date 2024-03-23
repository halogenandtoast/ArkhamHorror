module Arkham.Enemy.Cards.AtlachNacha (
  atlachNacha,
  AtlachNacha (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Data.Aeson (Result (..))

newtype Meta = Meta {rotation :: Int}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype AtlachNacha = AtlachNacha EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

atlachNacha :: EnemyCard AtlachNacha
atlachNacha =
  enemyWith
    AtlachNacha
    Cards.atlachNacha
    (0, Static 1, 0)
    (0, 0)
    ((asSelfLocationL ?~ "atlachNacha") . setMeta @Meta (Meta 0))

instance RunMessage AtlachNacha where
  runMessage msg (AtlachNacha attrs) = case msg of
    HuntersMove -> do
      let
        Meta n =
          case fromJSON @Meta attrs.meta of
            Success a -> a
            Error _ -> Meta 0
      pure $ AtlachNacha $ setMeta @Meta (Meta ((n + 45) `mod` 360)) attrs
    _ -> AtlachNacha <$> runMessage msg attrs
