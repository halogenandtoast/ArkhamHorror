module Arkham.Treachery.Cards.SlitheringBehindYou
  ( SlitheringBehindYou(..)
  , slitheringBehindYou
  ) where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Card
import Arkham.Classes
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.Target
import Arkham.Treachery.Attrs
import Arkham.Treachery.Runner

newtype SlitheringBehindYou = SlitheringBehindYou TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

slitheringBehindYou :: TreacheryCard SlitheringBehindYou
slitheringBehindYou = treachery SlitheringBehindYou Cards.slitheringBehindYou

instance TreacheryRunner env => RunMessage env SlitheringBehindYou where
  runMessage msg t@(SlitheringBehindYou attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      mHuntingHorrorId <- selectOne $ enemyIs Cards.huntingHorror
      case mHuntingHorrorId of
        Just eid ->
          t <$ pushAll
            [PlaceDoom (EnemyTarget eid) 1, ShuffleIntoEncounterDeck []]
        Nothing ->
          t
            <$ push
                 (FindEncounterCard
                   iid
                   (toTarget attrs)
                   (CardWithCardCode "02141")
                 )
    FoundEncounterCard iid target ec | isTarget attrs target -> do
      lid <- getId @LocationId iid
      t <$ push (SpawnEnemyAtEngagedWith (EncounterCard ec) lid iid)
    FoundEnemyInVoid iid target eid | isTarget attrs target -> do
      lid <- getId @LocationId iid
      t <$ push (EnemySpawnFromVoid (Just iid) lid eid)
    _ -> SlitheringBehindYou <$> runMessage msg attrs
