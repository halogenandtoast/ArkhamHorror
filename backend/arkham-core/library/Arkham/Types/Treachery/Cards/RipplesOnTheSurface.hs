{-# LANGUAGE UndecidableInstances #-}

module Arkham.Types.Treachery.Cards.RipplesOnTheSurface
  ( RipplesOnTheSurface(..)
  , ripplesOnTheSurface
  )
where

import Arkham.Import

import Arkham.Types.Trait
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Helpers
import Arkham.Types.Treachery.Runner

newtype RipplesOnTheSurface = RipplesOnTheSurface Attrs
  deriving newtype (Show, ToJSON, FromJSON)

ripplesOnTheSurface :: TreacheryId -> a -> RipplesOnTheSurface
ripplesOnTheSurface uuid _ = RipplesOnTheSurface $ baseAttrs uuid "81027"

instance
  ( HasId LocationId env InvestigatorId
  , HasSet Trait env LocationId
  )
  => HasModifiersFor env RipplesOnTheSurface where
  getModifiersFor (SkillTestSource _ _ source _) (InvestigatorTarget iid) (RipplesOnTheSurface attrs)
    | isSource attrs source
    = do
      locationId <- getId @LocationId iid
      isBayou <- member Bayou <$> getSet locationId
      pure $ toModifiers attrs [ CannotCommitCards | isBayou ]
  getModifiersFor _ _ _ = pure []

instance HasActions env RipplesOnTheSurface where
  getActions i window (RipplesOnTheSurface attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env RipplesOnTheSurface where
  runMessage msg t@(RipplesOnTheSurface attrs@Attrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      t <$ unshiftMessages
        [ RevelationSkillTest iid source SkillWillpower 3
        , Discard (TreacheryTarget treacheryId)
        ]
    FailedSkillTest iid _ (TreacherySource tid) SkillTestInitiatorTarget{} n
      | tid == treacheryId -> t <$ unshiftMessage
        (InvestigatorAssignDamage iid (TreacherySource treacheryId) 0 n)
    _ -> RipplesOnTheSurface <$> runMessage msg attrs
