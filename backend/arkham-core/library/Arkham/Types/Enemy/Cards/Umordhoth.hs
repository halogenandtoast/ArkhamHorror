module Arkham.Types.Enemy.Cards.Umordhoth
  ( Umordhoth(..)
  , umordhoth
  )
where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.EnemyId
import Arkham.Types.GameValue
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.Source
import Arkham.Types.Window
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Enemy.Runner

newtype Umordhoth = Umordhoth EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

umordhoth :: EnemyId -> Umordhoth
umordhoth uuid =
  Umordhoth
    $ baseAttrs uuid "01157"
    $ (healthDamageL .~ 3)
    . (sanityDamageL .~ 3)
    . (fightL .~ 5)
    . (healthL .~ Static 6)
    . (evadeL .~ 6)
    . (uniqueL .~ True)

instance HasModifiersFor env Umordhoth where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env Umordhoth where
  getActions iid NonFast (Umordhoth attrs@EnemyAttrs {..}) =
    withBaseActions iid NonFast attrs $ do
      maid <- fmap unStoryAssetId <$> getId (CardCode "01117")
      locationId <- getId @LocationId iid
      case maid of
        Nothing -> pure []
        Just aid -> do
          miid <- fmap unOwnerId <$> getId aid
          pure
            [ ActivateCardAbilityAction
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
      e <$ unshiftMessage (ScenarioResolution $ Resolution 3)
    _ -> Umordhoth <$> runMessage msg attrs
