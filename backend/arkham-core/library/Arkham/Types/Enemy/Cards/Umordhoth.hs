module Arkham.Types.Enemy.Cards.Umordhoth
  ( Umordhoth(..)
  , umordhoth
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Enemy.Runner
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.Source
import Arkham.Types.Window

newtype Umordhoth = Umordhoth EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

umordhoth :: EnemyCard Umordhoth
umordhoth = enemy Umordhoth Cards.umordhoth (5, Static 6, 6) (3, 3)

instance HasModifiersFor env Umordhoth

instance ActionRunner env => HasActions env Umordhoth where
  getActions iid NonFast (Umordhoth attrs@EnemyAttrs {..}) =
    withBaseActions iid NonFast attrs $ do
      maid <- selectOne (AssetIs Cards.litaChantler)
      locationId <- getId @LocationId iid
      case maid of
        Nothing -> pure []
        Just aid -> do
          miid <- fmap unOwnerId <$> getId aid
          pure
            [ UseAbility
                iid
                (mkAbility
                  (EnemySource enemyId)
                  1
                  (ActionAbility Nothing $ ActionCost 1)
                )
            | locationId == enemyLocation && miid == Just iid
            ]
  getActions i window (Umordhoth attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env Umordhoth where
  runMessage msg e@(Umordhoth attrs@EnemyAttrs {..}) = case msg of
    EnemySpawn _ _ eid | eid == enemyId -> do
      playerCount <- unPlayerCount <$> getCount ()
      Umordhoth
        <$> runMessage msg (attrs & healthL %~ fmap (+ (4 * playerCount)))
    ChooseEndTurn _ ->
      Umordhoth <$> runMessage msg (attrs & exhaustedL .~ False)
    UseCardAbility _ (EnemySource eid) _ 1 _ | eid == enemyId ->
      e <$ push (ScenarioResolution $ Resolution 3)
    _ -> Umordhoth <$> runMessage msg attrs
