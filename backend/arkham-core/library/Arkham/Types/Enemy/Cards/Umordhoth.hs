{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.Umordhoth where

import Arkham.Import

import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner

newtype Umordhoth = Umordhoth Attrs
  deriving newtype (Show, ToJSON, FromJSON)

umordhoth :: EnemyId -> Umordhoth
umordhoth uuid =
  Umordhoth
    $ baseAttrs uuid "01157"
    $ (healthDamage .~ 3)
    . (sanityDamage .~ 3)
    . (fight .~ 5)
    . (health .~ Static 6)
    . (evade .~ 6)

instance HasModifiersFor env Umordhoth where
  getModifiersFor = noModifiersFor

instance HasModifiers env Umordhoth where
  getModifiers _ (Umordhoth Attrs {..}) =
    pure . concat . toList $ enemyModifiers

instance ActionRunner env => HasActions env Umordhoth where
  getActions iid NonFast (Umordhoth attrs@Attrs {..}) = do
    baseActions <- getActions iid NonFast attrs
    maid <- asks (fmap unStoryAssetId <$> getId (CardCode "01117"))
    locationId <- asks $ getId @LocationId iid
    case maid of
      Nothing -> pure baseActions
      Just aid -> do
        miid <- fmap unOwnerId <$> asks (getId aid)
        pure
          $ baseActions
          <> [ ActivateCardAbilityAction
                 iid
                 (mkAbility (EnemySource enemyId) 1 (ActionAbility 1 Nothing))
             | locationId == enemyLocation && miid == Just iid
             ]
  getActions i window (Umordhoth attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env Umordhoth where
  runMessage msg e@(Umordhoth attrs@Attrs {..}) = case msg of
    EnemySpawn _ eid | eid == enemyId -> do
      playerCount <- unPlayerCount <$> asks (getCount ())
      Umordhoth
        <$> runMessage msg (attrs & health %~ fmap (+ (4 * playerCount)))
    ChooseEndTurn _ -> do
      Umordhoth <$> runMessage msg (attrs & exhausted .~ False)
    UseCardAbility _ (EnemySource eid) _ 1 | eid == enemyId ->
      e <$ unshiftMessage (Resolution 3)
    _ -> Umordhoth <$> runMessage msg attrs
