module Arkham.Enemy.Cards.JoyceLittleBookshopOwner
  ( joyceLittleBookshopOwner
  , JoyceLittleBookshopOwner(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype JoyceLittleBookshopOwner = JoyceLittleBookshopOwner EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

joyceLittleBookshopOwner :: EnemyCard JoyceLittleBookshopOwner
joyceLittleBookshopOwner = enemy JoyceLittleBookshopOwner Cards.joyceLittleBookshopOwner (5, Static 3, 3) (0, 1)

instance RunMessage JoyceLittleBookshopOwner where
  runMessage msg (JoyceLittleBookshopOwner attrs) =
    JoyceLittleBookshopOwner <$> runMessage msg attrs
