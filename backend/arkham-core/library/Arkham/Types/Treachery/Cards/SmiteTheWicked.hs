module Arkham.Types.Treachery.Cards.SmiteTheWicked where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype SmiteTheWicked = SmiteTheWicked TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasActions)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

smiteTheWicked :: TreacheryCard SmiteTheWicked
smiteTheWicked = treachery SmiteTheWicked Cards.smiteTheWicked

instance TreacheryRunner env => RunMessage env SmiteTheWicked where
  runMessage msg t@(SmiteTheWicked attrs@TreacheryAttrs {..}) = case msg of
    Revelation _iid source | isSource attrs source ->
      t <$ push (DiscardEncounterUntilFirst source (CardWithType EnemyType))
    RequestedEncounterCard (TreacherySource tid) mcard | tid == treacheryId ->
      case mcard of
        Nothing -> t <$ push (Discard $ toTarget attrs)
        Just card -> do
          let
            ownerId = fromJustNote "has to be set" treacheryOwner
            enemyId = EnemyId $ toCardId card
          farthestLocations <- map unFarthestLocationId <$> getSetList ownerId
          t <$ pushAll
            [ CreateEnemy (EncounterCard card)
            , AttachTreachery treacheryId (EnemyTarget enemyId)
            , chooseOne
              ownerId
              [ EnemySpawn Nothing lid enemyId | lid <- farthestLocations ]
            ]
    InvestigatorEliminated iid | treacheryOwner == Just iid ->
      runMessage EndOfGame t >>= \case
        SmiteTheWicked attrs' -> SmiteTheWicked <$> runMessage msg attrs'
    EndOfGame ->
      let investigator = fromJustNote "missing investigator" treacheryOwner
      in t <$ push (SufferTrauma investigator 0 1)
    _ -> SmiteTheWicked <$> runMessage msg attrs
