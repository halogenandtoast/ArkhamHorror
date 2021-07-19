module Arkham.Types.Enemy.Cards.Cnidathqua
  ( cnidathqua
  , Cnidathqua(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Resolution
import Arkham.Types.Target

newtype Cnidathqua = Cnidathqua EnemyAttrs
    deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cnidathqua :: EnemyCard Cnidathqua
cnidathqua = enemyWith
  Cnidathqua
  Cards.cnidathqua
  (0, Static 1, 0)
  (0, 0)
  (asSelfLocationL ?~ "cnidathqua")

instance HasModifiersFor env Cnidathqua where
  getModifiersFor _ (EnemyTarget eid) (Cnidathqua attrs) | eid == toId attrs =
    pure $ toModifiers attrs [CannotBeEvaded, CanBeFoughtAsIfAtYourLocation]
  getModifiersFor _ _ _ = pure []

instance EnemyAttrsHasActions env => HasActions env Cnidathqua where
  getActions i window (Cnidathqua attrs) = getActions i window attrs

instance EnemyAttrsRunMessage env => RunMessage env Cnidathqua where
  runMessage msg e@(Cnidathqua attrs) = case msg of
    EnemyDefeated eid _ _ _ _ _ | eid == enemyId attrs -> do
      e <$ push (ScenarioResolution $ Resolution 2)
    _ -> Cnidathqua <$> runMessage msg attrs
