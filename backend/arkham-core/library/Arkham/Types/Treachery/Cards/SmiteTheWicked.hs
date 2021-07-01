module Arkham.Types.Treachery.Cards.SmiteTheWicked where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype SmiteTheWicked = SmiteTheWicked TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

smiteTheWicked :: TreacheryCard SmiteTheWicked
smiteTheWicked = treachery SmiteTheWicked Cards.smiteTheWicked

instance HasModifiersFor env SmiteTheWicked where
  getModifiersFor = noModifiersFor

instance HasActions env SmiteTheWicked where
  getActions i window (SmiteTheWicked attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env SmiteTheWicked where
  runMessage msg t@(SmiteTheWicked attrs@TreacheryAttrs {..}) = case msg of
    Revelation _iid source | isSource attrs source -> t <$ unshiftMessage
      (DiscardEncounterUntilFirst
        source
        (CardMatchByType (EnemyType, mempty))
      )
    RequestedEncounterCard (TreacherySource tid) mcard | tid == treacheryId ->
      case mcard of
        Nothing -> t <$ unshiftMessage (Discard $ toTarget attrs)
        Just card -> do
          let
            ownerId = fromJustNote "has to be set" treacheryOwner
            enemyId = EnemyId $ toCardId card
          farthestLocations <- map unFarthestLocationId <$> getSetList ownerId
          t <$ unshiftMessages
            [ CreateEnemy (EncounterCard card)
            , AttachTreachery treacheryId (EnemyTarget enemyId)
            , chooseOne
              ownerId
              [ EnemySpawn (Just ownerId) lid enemyId
              | lid <- farthestLocations
              ]
            ]
    InvestigatorEliminated iid | treacheryOwner == Just iid ->
      runMessage EndOfGame t >>= \case
        SmiteTheWicked attrs' -> SmiteTheWicked <$> runMessage msg attrs'
    EndOfGame ->
      let investigator = fromJustNote "missing investigator" treacheryOwner
      in t <$ unshiftMessage (SufferTrauma investigator 0 1)
    _ -> SmiteTheWicked <$> runMessage msg attrs
