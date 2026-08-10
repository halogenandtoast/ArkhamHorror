module Arkham.Helpers.Name (module Arkham.Helpers.Name, module Arkham.Name) where

import Arkham.Card
import Arkham.Classes.Entity
import Arkham.Classes.HasGame
import Arkham.Enemy.Types
import Arkham.Helpers.FetchCard
import Arkham.Id
import Arkham.Name
import Arkham.Prelude
import Arkham.Projection

class GetName a where
  type NameRoot a :: Type
  nameField :: Field (NameRoot a) Name

getName
  :: forall a m
   . (GetName a, HasGame m, Projection (NameRoot a), AsId a, IdOf a ~ EntityId (NameRoot a))
  => a -> m Name
getName = field (nameField @a) . asId

instance GetName EnemyId where
  type NameRoot EnemyId = Enemy
  nameField = EnemyName

class HasFormatted a where
  getFormatted :: (HasGame m, CardGen m) => a -> m Text

instance HasFormatted EnemyId where
  getFormatted eid = do
    name <- getName eid
    cardCode <- toCardCode <$> fetchCard eid
    pure $ "{enemy:" <> tshow (toTitle name) <> ":" <> tshow eid <> ":" <> tshow cardCode <> "}"
