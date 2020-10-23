{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.DraggedUnder where

import Arkham.Import

import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype DraggedUnder = DraggedUnder Attrs
  deriving newtype (Show, ToJSON, FromJSON)

draggedUnder :: TreacheryId -> a -> DraggedUnder
draggedUnder uuid _ = DraggedUnder $ baseAttrs uuid "81026"

instance HasModifiersFor env DraggedUnder where
  getModifiersFor _ _ _ = pure []

instance HasActions env DraggedUnder where
  getActions i window (DraggedUnder attrs) = getActions i window attrs

instance (TreacheryRunner env) => RunMessage env DraggedUnder where
  runMessage msg t@(DraggedUnder attrs@Attrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      unshiftMessage $ RevelationSkillTest
        iid
        source
        SkillAgility
        3
        [Discard (TreacheryTarget treacheryId)]
        [AttachTreachery treacheryId (InvestigatorTarget iid)]
        []
      DraggedUnder <$> runMessage msg attrs
    MoveFrom iid _ | Just iid == treacheryAttachedInvestigator ->
      t <$ unshiftMessages
        [ InvestigatorAssignDamage iid (TreacherySource treacheryId) 2 0
        , Discard (TreacheryTarget treacheryId)
        ]
    EndTurn iid | Just iid == treacheryAttachedInvestigator ->
      t <$ unshiftMessage
        (BeginSkillTest
          iid
          (TreacherySource treacheryId)
          (InvestigatorTarget iid)
          Nothing
          SkillAgility
          3
          [Discard (TreacheryTarget treacheryId)]
          []
          []
          []
        )
    _ -> DraggedUnder <$> runMessage msg attrs
