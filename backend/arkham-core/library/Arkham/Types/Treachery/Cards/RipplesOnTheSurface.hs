{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.RipplesOnTheSurface
  ( RipplesOnTheSurface(..)
  , ripplesOnTheSurface
  )
where

import Arkham.Import

import Arkham.Types.Trait
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype RipplesOnTheSurface = RipplesOnTheSurface Attrs
  deriving newtype (Show, ToJSON, FromJSON)

ripplesOnTheSurface :: TreacheryId -> a -> RipplesOnTheSurface
ripplesOnTheSurface uuid _ = RipplesOnTheSurface $ baseAttrs uuid "81027"

instance HasModifiersFor env RipplesOnTheSurface where
  getModifiersFor _ _ _ = pure []

instance HasActions env RipplesOnTheSurface where
  getActions i window (RipplesOnTheSurface attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env RipplesOnTheSurface where
  runMessage msg t@(RipplesOnTheSurface attrs@Attrs {..}) = case msg of
    Revelation iid tid | tid == treacheryId -> do
      locationId <- asks $ getId @LocationId iid
      isBayou <- asks $ member Bayou . getSet locationId
      unshiftMessages
        [ RevelationSkillTest
          iid
          (TreacherySource treacheryId)
          SkillWillpower
          3
          []
          []
          [ CannotCommitCards | isBayou ]
        , Discard (TreacheryTarget tid)
        ]
      RipplesOnTheSurface <$> runMessage msg (attrs & resolved .~ True)
    FailedSkillTest iid _ (TreacherySource tid) SkillTestInitiatorTarget n
      | tid == treacheryId -> t <$ unshiftMessage
        (InvestigatorAssignDamage iid (TreacherySource treacheryId) 0 n)
    _ -> RipplesOnTheSurface <$> runMessage msg attrs
