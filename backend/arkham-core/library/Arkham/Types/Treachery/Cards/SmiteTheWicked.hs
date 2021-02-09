module Arkham.Types.Treachery.Cards.SmiteTheWicked where


import Arkham.Types.Card.EncounterCardMatcher
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype SmiteTheWicked = SmiteTheWicked TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

smiteTheWicked :: TreacheryId -> Maybe InvestigatorId -> SmiteTheWicked
smiteTheWicked uuid iid = SmiteTheWicked $ weaknessAttrs uuid iid "02007"

instance HasModifiersFor env SmiteTheWicked where
  getModifiersFor = noModifiersFor

instance HasActions env SmiteTheWicked where
  getActions i window (SmiteTheWicked attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env SmiteTheWicked where
  runMessage msg t@(SmiteTheWicked attrs@TreacheryAttrs {..}) = case msg of
    Revelation _iid source | isSource attrs source -> t <$ unshiftMessage
      (DiscardEncounterUntilFirst
        source
        (EncounterCardMatchByType (EnemyType, Nothing))
      )
    RequestedEncounterCard (TreacherySource tid) mcard | tid == treacheryId ->
      case mcard of
        Nothing -> t <$ unshiftMessage (Discard $ toTarget attrs)
        Just card -> t <$ unshiftMessage
          (CreateEnemyRequest (TreacherySource tid) (EncounterCard card))
    RequestedEnemy (TreacherySource tid) eid | tid == treacheryId -> do
      let ownerId = fromJustNote "has to be set" treacheryOwner
      farthestLocations <- map unFarthestLocationId <$> getSetList ownerId
      t <$ unshiftMessages
        [ AttachTreachery treacheryId (EnemyTarget eid)
        , chooseOne
          ownerId
          [ EnemySpawn (Just ownerId) lid eid | lid <- farthestLocations ]
        ]
    InvestigatorEliminated iid | treacheryOwner == Just iid ->
      runMessage EndOfGame t >>= \case
        SmiteTheWicked attrs' -> SmiteTheWicked <$> runMessage msg attrs'
    EndOfGame ->
      let investigator = fromJustNote "missing investigator" treacheryOwner
      in t <$ unshiftMessage (SufferTrauma investigator 0 1)
    _ -> SmiteTheWicked <$> runMessage msg attrs
