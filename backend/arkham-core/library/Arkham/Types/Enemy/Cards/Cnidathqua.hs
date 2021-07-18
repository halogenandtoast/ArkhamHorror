module Arkham.Types.Enemy.Cards.Cnidathqua (
    cnidathqua,
    Cnidathqua (..),
) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs

newtype Cnidathqua = Cnidathqua EnemyAttrs
    deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cnidathqua :: EnemyCard Cnidathqua
cnidathqua = enemyWith Cnidathqua Cards.cnidathqua (0, Static 1, 0) (0, 0) (asSelfLocationL ?~ "cnidathqua")

instance HasModifiersFor env Cnidathqua

instance EnemyAttrsHasActions env => HasActions env Cnidathqua where
    getActions i window (Cnidathqua attrs) = getActions i window attrs

instance EnemyAttrsRunMessage env => RunMessage env Cnidathqua where
    runMessage msg (Cnidathqua attrs) =
        Cnidathqua <$> runMessage msg attrs
