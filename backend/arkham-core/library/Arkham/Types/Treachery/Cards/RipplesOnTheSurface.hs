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

instance
  ( HasId LocationId InvestigatorId env
  , HasSet Trait LocationId env
  )
  => HasModifiersFor env RipplesOnTheSurface where
  getModifiersFor (SkillTestSource _ source _) (InvestigatorTarget iid) (RipplesOnTheSurface attrs)
    | isSource attrs source
    = do
      locationId <- asks $ getId @LocationId iid
      isBayou <- asks $ member Bayou . getSet locationId
      pure [ CannotCommitCards | isBayou ]
  getModifiersFor _ _ _ = pure []

instance HasActions env RipplesOnTheSurface where
  getActions i window (RipplesOnTheSurface attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env RipplesOnTheSurface where
  runMessage msg t@(RipplesOnTheSurface attrs@Attrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      unshiftMessages
        [ RevelationSkillTest iid source SkillWillpower 3
        , Discard (TreacheryTarget treacheryId)
        ]
      RipplesOnTheSurface <$> runMessage msg (attrs & resolved .~ True)
    FailedSkillTest iid _ (TreacherySource tid) SkillTestInitiatorTarget{} n
      | tid == treacheryId -> t <$ unshiftMessage
        (InvestigatorAssignDamage iid (TreacherySource treacheryId) 0 n)
    _ -> RipplesOnTheSurface <$> runMessage msg attrs
