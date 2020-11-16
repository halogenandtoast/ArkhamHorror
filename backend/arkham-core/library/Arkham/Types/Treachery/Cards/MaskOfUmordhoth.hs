{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.MaskOfUmordhoth where

import Arkham.Import hiding (Cultist)

import Arkham.Types.Card.EncounterCardMatcher
import qualified Arkham.Types.Keyword as Keyword
import Arkham.Types.Trait
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype MaskOfUmordhoth = MaskOfUmordhoth Attrs
  deriving newtype (Show, ToJSON, FromJSON)

maskOfUmordhoth :: TreacheryId -> a -> MaskOfUmordhoth
maskOfUmordhoth uuid _ = MaskOfUmordhoth $ baseAttrs uuid "50043"

instance HasModifiersFor env MaskOfUmordhoth where
  getModifiersFor _ (EnemyTarget eid) (MaskOfUmordhoth attrs)
    | Just eid == treacheryAttachedEnemy attrs = pure [HealthModifier 2]
  getModifiersFor _ _ _ = pure []

instance HasActions env MaskOfUmordhoth where
  getActions i window (MaskOfUmordhoth attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env MaskOfUmordhoth where
  runMessage msg (MaskOfUmordhoth attrs@Attrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      enemies <- asks $ map unFarthestEnemyId . setToList . getSet
        (iid, EnemyTrait Cultist)
      case enemies of
        [] -> unshiftMessages
          [ FindAndDrawEncounterCard
            iid
            (EncounterCardMatchByType (EnemyType, Just Cultist))
          , Revelation iid source
          ]
        [eid] -> unshiftMessage (AttachTreachery treacheryId (EnemyTarget eid))
        eids -> unshiftMessage
          (chooseOne
            iid
            [ AttachTreachery treacheryId (EnemyTarget eid) | eid <- eids ]
          )
      MaskOfUmordhoth <$> runMessage msg (attrs & resolved .~ False)
    AttachTreachery tid target@(EnemyTarget eid) | tid == treacheryId -> do
      uniqueEnemyIds <- asks $ map unUniqueEnemyId . setToList . getSet ()
      let
        keyword =
          if eid `elem` uniqueEnemyIds then Keyword.Retaliate else Keyword.Aloof
      unshiftMessage (AddKeywords target [keyword])
      MaskOfUmordhoth <$> runMessage msg attrs
    Discard (TreacheryTarget tid) | tid == treacheryId -> do
      case treacheryAttachedEnemy of
        Just eid -> do
          uniqueEnemyIds <- asks $ map unUniqueEnemyId . setToList . getSet ()
          let
            keyword = if eid `elem` uniqueEnemyIds
              then Keyword.Retaliate
              else Keyword.Aloof
          unshiftMessage (RemoveKeywords (EnemyTarget eid) [keyword])
        Nothing -> pure ()
      MaskOfUmordhoth <$> runMessage msg attrs
    _ -> MaskOfUmordhoth <$> runMessage msg attrs
