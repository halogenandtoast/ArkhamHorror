{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.CursedSwamp
  ( CursedSwamp(..)
  , cursedSwamp
  )
where

import Arkham.Import

import Arkham.Types.Trait
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype CursedSwamp = CursedSwamp Attrs
  deriving newtype (Show, ToJSON, FromJSON)

cursedSwamp :: TreacheryId -> a -> CursedSwamp
cursedSwamp uuid _ = CursedSwamp $ baseAttrs uuid "81024"

instance HasModifiersFor env CursedSwamp where
  getModifiersFor _ _ _ = pure []

instance HasActions env CursedSwamp where
  getActions i window (CursedSwamp attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env CursedSwamp where
  runMessage msg t@(CursedSwamp attrs@Attrs {..}) = case msg of
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
      CursedSwamp <$> runMessage msg (attrs & resolved .~ True)
    FailedSkillTest iid _ (TreacherySource tid) SkillTestInitiatorTarget n
      | tid == treacheryId -> t <$ unshiftMessage
        (InvestigatorAssignDamage iid (TreacherySource treacheryId) n 0)
    _ -> CursedSwamp <$> runMessage msg attrs
