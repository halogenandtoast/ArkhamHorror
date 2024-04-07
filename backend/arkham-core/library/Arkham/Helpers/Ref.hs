module Arkham.Helpers.Ref where

import Arkham.Asset.Types (Field (..))
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Enemy.Types (Field (..))
import Arkham.Event.Types (Field (..))
import Arkham.Game.Helpers
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Location.Types (Field (..))
import Arkham.Prelude
import Arkham.Projection
import Arkham.Skill.Types (Field (..))
import Arkham.Source
import Arkham.Target
import Arkham.Treachery.Types (Field (..))

targetToCard :: (HasCallStack, HasGame m) => Target -> m Card
targetToCard target = fromMaybe handleMissing <$> targetToMaybeCard target
 where
  handleMissing = error $ "unhandled: " <> show target

targetToMaybeCard :: (HasCallStack, HasGame m) => Target -> m (Maybe Card)
targetToMaybeCard = \case
  AssetTarget aid -> Just <$> field AssetCard aid
  EventTarget aid -> Just <$> field EventCard aid
  SkillTarget aid -> Just <$> field SkillCard aid
  EnemyTarget aid -> Just <$> field EnemyCard aid
  TreacheryTarget aid -> Just <$> field TreacheryCard aid
  LocationTarget aid -> Just <$> field LocationCard aid
  CardTarget c -> pure $ Just c
  SearchedCardTarget cId -> Just <$> getCard cId
  CardIdTarget cId -> Just <$> getCard cId
  _ -> pure Nothing

sourceToCard :: (HasCallStack, HasGame m) => Source -> m Card
sourceToCard = targetToCard . sourceToTarget

sourceToMaybeCard :: (HasCallStack, HasGame m) => Source -> m (Maybe Card)
sourceToMaybeCard = targetToMaybeCard . sourceToTarget
