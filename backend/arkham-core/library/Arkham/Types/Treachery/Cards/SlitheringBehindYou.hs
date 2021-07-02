module Arkham.Types.Treachery.Cards.SlitheringBehindYou
  ( SlitheringBehindYou(..)
  , slitheringBehindYou
  ) where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype SlitheringBehindYou = SlitheringBehindYou TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

slitheringBehindYou :: TreacheryCard SlitheringBehindYou
slitheringBehindYou = treachery SlitheringBehindYou Cards.slitheringBehindYou

instance HasModifiersFor env SlitheringBehindYou where
  getModifiersFor = noModifiersFor

instance HasActions env SlitheringBehindYou where
  getActions i window (SlitheringBehindYou attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env SlitheringBehindYou where
  runMessage msg t@(SlitheringBehindYou attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      mHuntingHorrorId <- fmap unStoryEnemyId <$> getId (CardCode "02141")
      case mHuntingHorrorId of
        Just eid -> t <$ unshiftMessages
          [ PlaceDoom (EnemyTarget eid) 1
          , ShuffleIntoEncounterDeck []
          , Discard $ toTarget attrs
          ]
        Nothing -> t <$ unshiftMessage
          (FindEncounterCard
            iid
            (toTarget attrs)
            (CardMatchByCardCode "02141")
          )
    FoundEncounterCard iid target ec | isTarget attrs target -> do
      lid <- getId @LocationId iid
      t <$ unshiftMessage (SpawnEnemyAtEngagedWith (EncounterCard ec) lid iid)
    FoundEnemyInVoid iid target eid | isTarget attrs target -> do
      lid <- getId @LocationId iid
      t <$ unshiftMessage (EnemySpawnFromVoid (Just iid) lid eid)
    _ -> SlitheringBehindYou <$> runMessage msg attrs
