module Arkham.Types.Enemy.Cards.WrithingAppendage
  ( writhingAppendage
  , WrithingAppendage(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Id
import Arkham.Types.Message

newtype WrithingAppendage = WrithingAppendage EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

writhingAppendage :: EnemyCard WrithingAppendage
writhingAppendage =
  enemy WrithingAppendage Cards.writhingAppendage (2, Static 2, 4) (1, 0)

instance HasModifiersFor env WrithingAppendage

instance EnemyAttrsHasActions env => HasActions env WrithingAppendage where
  getActions i window (WrithingAppendage attrs) = getActions i window attrs

instance
  ( HasId (Maybe StoryEnemyId) env CardCode
  , EnemyAttrsRunMessage env
  )
  => RunMessage env WrithingAppendage where
  runMessage msg e@(WrithingAppendage attrs) = case msg of
    After (EnemyAttack iid eid _) | eid == toId attrs ->
      e <$ push (RandomDiscard iid)
    EnemyDefeated eid iid _ _ _ _ | eid == toId attrs -> do
      mCnidathquaId <- fmap unStoryEnemyId
        <$> getId (toCardCode Cards.cnidathqua)
      case mCnidathquaId of
        Just cnidathquaId ->
          push (EnemyDamage cnidathquaId iid (toSource attrs) 1)
        Nothing -> pure ()
      WrithingAppendage <$> runMessage msg attrs
    _ -> WrithingAppendage <$> runMessage msg attrs
