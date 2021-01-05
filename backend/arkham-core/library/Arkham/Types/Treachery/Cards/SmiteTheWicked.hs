module Arkham.Types.Treachery.Cards.SmiteTheWicked where

import Arkham.Import

import Arkham.Types.Card.EncounterCardMatcher
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype SmiteTheWicked = SmiteTheWicked Attrs
  deriving newtype (Show, ToJSON, FromJSON)

smiteTheWicked :: TreacheryId -> Maybe InvestigatorId -> SmiteTheWicked
smiteTheWicked uuid iid = SmiteTheWicked $ weaknessAttrs uuid iid "02007"

instance HasModifiersFor env SmiteTheWicked where
  getModifiersFor = noModifiersFor

instance HasActions env SmiteTheWicked where
  getActions i window (SmiteTheWicked attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env SmiteTheWicked where
  runMessage msg t@(SmiteTheWicked attrs@Attrs {..}) = case msg of
    Revelation _iid source | isSource attrs source -> do
      t <$ unshiftMessage
        (DiscardEncounterUntilFirst
          source
          (EncounterCardMatchByType (EnemyType, Nothing))
        )
    RequestedEncounterCard (TreacherySource tid) mcard | tid == treacheryId ->
      do
        case mcard of
          Nothing -> t <$ unshiftMessage (Discard (TreacheryTarget tid))
          Just card -> t <$ unshiftMessage
            (CreateEnemyRequest (TreacherySource tid) (ecCardCode card))
    RequestedEnemy (TreacherySource tid) eid | tid == treacheryId -> do
      let ownerId = fromJustNote "has to be set" treacheryOwner
      farthestLocations <- map unFarthestLocationId <$> getSetList ownerId
      t <$ unshiftMessages
        [ AttachTreachery treacheryId (EnemyTarget eid)
        , Ask
          ownerId
          (ChooseOne
            [ EnemySpawn (Just ownerId) lid eid | lid <- farthestLocations ]
          )
        ]
    InvestigatorEliminated iid | treacheryOwner == Just iid -> do
      runMessage EndOfGame t >>= \case
        SmiteTheWicked attrs' -> SmiteTheWicked <$> runMessage msg attrs'
    EndOfGame ->
      let investigator = fromJustNote "missing investigator" treacheryOwner
      in t <$ unshiftMessage (SufferTrauma investigator 0 1)
    _ -> SmiteTheWicked <$> runMessage msg attrs
