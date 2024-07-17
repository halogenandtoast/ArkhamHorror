module Arkham.Helpers.EncounterCard where

import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Classes.Query
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.Prelude
import Arkham.Source

changeEncounterCardDrawer :: (HasQueue Message m, HasGame m) => CardId -> InvestigatorId -> m ()
changeEncounterCardDrawer cardId iid = do
  mTreachery <- selectOne $ TreacheryWithCardId cardId
  mEnemy <- selectOne $ EnemyWithCardId cardId
  mLocation <- selectOne $ LocationWithCardId cardId
  mAsset <- selectOne $ AssetWithCardId cardId
  mEvent <- selectOne $ EventWithCardId cardId

  let
    baseReplace :: Sourceable source => source -> Message -> Message
    baseReplace (toSource -> source) = \case
      Revelation _ source' | source == source' -> Revelation iid source
      ResolvedCard _ encounterCard | toCardId encounterCard == cardId -> ResolvedCard iid encounterCard
      Surge _ source' | source == source' -> Surge iid source
      other -> other

  for_ mTreachery \tid -> do
    withQueue_ $ map $ \case
      AfterRevelation _ tid' | tid == tid' -> AfterRevelation iid tid'
      other -> baseReplace tid other

  for_ mEnemy \enemyId -> do
    withQueue_ $ map $ \case
      InvestigatorDrawEnemy _ enemyId' | enemyId == enemyId' -> InvestigatorDrawEnemy iid enemyId'
      other -> baseReplace enemyId other

  for_ mLocation \tid -> withQueue_ $ map (baseReplace tid)
  for_ mAsset \aid -> withQueue_ $ map (baseReplace aid)
  for_ mEvent \aid -> withQueue_ $ map (baseReplace aid)

  when (isNothing $ (mTreachery $> ()) <|> (mEnemy $> ()) <|> (mLocation $> ()) <|> (mAsset $> ()))
    $ error "Unhandled card type"
