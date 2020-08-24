{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.Umordhoth where

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.AssetId
import Arkham.Types.Card.CardCode
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.EnemyId
import Arkham.Types.FastWindow
import Arkham.Types.GameValue
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Source
import ClassyPrelude
import Lens.Micro

newtype Umordhoth = Umordhoth Attrs
  deriving newtype (Show, ToJSON, FromJSON)

umordhoth :: EnemyId -> Umordhoth
umordhoth uuid = Umordhoth $ (baseAttrs uuid "01157")
  { enemyHealthDamage = 3
  , enemySanityDamage = 3
  , enemyFight = 5
  , enemyHealth = Static 6
  , enemyEvade = 6
  }

instance (ActionRunner env investigator) => HasActions env investigator Umordhoth where
  getActions i NonFast (Umordhoth attrs@Attrs {..}) = do
    baseActions <- getActions i NonFast attrs
    maid <- asks (fmap unStoryAssetId <$> getId (CardCode "01117"))
    case maid of
      Nothing -> pure baseActions
      Just aid -> do
        miid <- fmap unOwnerId <$> asks (getId aid)
        pure
          $ baseActions
          <> [ ActivateCardAbilityAction
                 (getId () i)
                 (mkAbility (EnemySource enemyId) 1 (ActionAbility 1 Nothing))
             | locationOf i == enemyLocation && miid == Just (getId () i)
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
    UseCardAbility _ _ (EnemySource eid) 1 | eid == enemyId ->
      e <$ unshiftMessage (Resolution 3)
    _ -> Umordhoth <$> runMessage msg attrs
