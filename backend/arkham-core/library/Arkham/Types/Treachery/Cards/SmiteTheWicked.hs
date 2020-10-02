{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.SmiteTheWicked where

import Arkham.Json
import Arkham.Types.Card.EncounterCard
import Arkham.Types.Classes
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner
import Arkham.Types.TreacheryId
import ClassyPrelude
import qualified Data.HashSet as HashSet
import Lens.Micro
import Safe (fromJustNote)

newtype SmiteTheWicked = SmiteTheWicked Attrs
  deriving newtype (Show, ToJSON, FromJSON)

smiteTheWicked :: TreacheryId -> Maybe InvestigatorId -> SmiteTheWicked
smiteTheWicked uuid iid = SmiteTheWicked $ weaknessAttrs uuid iid "02007"

instance HasActions env investigator SmiteTheWicked where
  getActions i window (SmiteTheWicked attrs) = getActions i window attrs

instance (TreacheryRunner env) => RunMessage env SmiteTheWicked where
  runMessage msg t@(SmiteTheWicked attrs@Attrs {..}) = case msg of
    Revelation _iid tid | tid == treacheryId -> do
      unshiftMessage
        (DiscardEncounterUntilFirst (TreacherySource tid) (EnemyType, Nothing))
      SmiteTheWicked <$> runMessage msg (attrs & resolved .~ False)
    RequestedEncounterCard (TreacherySource tid) mcard | tid == treacheryId ->
      do
        case mcard of
          Nothing -> t <$ unshiftMessage (Discard (TreacheryTarget tid))
          Just card -> t <$ unshiftMessage
            (CreateEnemyRequest (TreacherySource tid) (ecCardCode card))
    RequestedEnemy (TreacherySource tid) eid | tid == treacheryId -> do
      let ownerId = fromJustNote "has to be set" treacheryOwner
      farthestLocations <- setToList . HashSet.map unFarthestLocationId <$> asks
        (getSet ownerId)
      t <$ unshiftMessages
        [ AttachTreachery treacheryId (EnemyTarget eid)
        , Ask
          ownerId
          (ChooseOne [ EnemySpawn lid eid | lid <- farthestLocations ])
        ]
    InvestigatorEliminated iid | treacheryOwner == Just iid -> do
      runMessage EndOfGame t >>= \case
        SmiteTheWicked attrs' -> SmiteTheWicked <$> runMessage msg attrs'
    EndOfGame ->
      let investigator = fromJustNote "missing investigator" treacheryOwner
      in t <$ unshiftMessage (SufferTrauma investigator 0 1)
    _ -> SmiteTheWicked <$> runMessage msg attrs
