module Arkham.Types.Treachery.Cards.SlitheringBehindYou
  ( SlitheringBehindYou(..)
  , slitheringBehindYou
  ) where

import Arkham.Import

import Arkham.Types.Card.EncounterCardMatcher
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype SlitheringBehindYou = SlitheringBehindYou Attrs
  deriving newtype (Show, ToJSON, FromJSON)

slitheringBehindYou :: TreacheryId -> a -> SlitheringBehindYou
slitheringBehindYou uuid _ = SlitheringBehindYou $ baseAttrs uuid "02146"

instance HasModifiersFor env SlitheringBehindYou where
  getModifiersFor = noModifiersFor

instance HasActions env SlitheringBehindYou where
  getActions i window (SlitheringBehindYou attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env SlitheringBehindYou where
  runMessage msg t@(SlitheringBehindYou attrs@Attrs {..}) = case msg of
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
            (EncounterCardMatchByCardCode "02141")
          )
    FoundEncounterCard iid target ec | isTarget attrs target -> do
      lid <- getId @LocationId iid
      t <$ unshiftMessage (SpawnEnemyAtEngagedWith (EncounterCard ec) lid iid)
    FoundEnemyInVoid iid target eid | isTarget attrs target -> do
      lid <- getId @LocationId iid
      t <$ unshiftMessage (SpawnEnemyAtEngagedWith (EncounterCard ec) lid iid)
    _ -> SlitheringBehindYou <$> runMessage msg attrs
