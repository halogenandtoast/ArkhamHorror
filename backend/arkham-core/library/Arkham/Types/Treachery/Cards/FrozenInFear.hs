{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.FrozenInFear where

import Arkham.Json
import qualified Arkham.Types.Action as Action
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner
import Arkham.Types.TreacheryId
import ClassyPrelude
import Lens.Micro

newtype FrozenInFear = FrozenInFear Attrs
  deriving newtype (Show, ToJSON, FromJSON)

frozenInFear :: TreacheryId -> a -> FrozenInFear
frozenInFear uuid _ = FrozenInFear $ baseAttrs uuid "01164"

instance HasModifiersFor env FrozenInFear where
  getModifiersFor _ _ _ = pure []

instance HasActions env FrozenInFear where
  getActions i window (FrozenInFear attrs) = getActions i window attrs

instance (TreacheryRunner env) => RunMessage env FrozenInFear where
  runMessage msg t@(FrozenInFear attrs@Attrs {..}) = case msg of
    Revelation iid tid | tid == treacheryId -> do
      unshiftMessages
        [ AttachTreachery tid (InvestigatorTarget iid)
        , AddModifiers
          (InvestigatorTarget iid)
          (TreacherySource tid)
          [ ActionCostOf
              (FirstOneOf [Action.Move, Action.Fight, Action.Evade])
              1
          ]
        ]
      FrozenInFear <$> runMessage msg (attrs & attachedInvestigator ?~ iid)
    ChooseEndTurn iid | Just iid == treacheryAttachedInvestigator ->
      t <$ unshiftMessage
        (RevelationSkillTest
          iid
          (TreacherySource treacheryId)
          SkillWillpower
          3
          [ RemoveAllModifiersOnTargetFrom
            (InvestigatorTarget iid)
            (TreacherySource treacheryId)
          , Discard (TreacheryTarget treacheryId)
          ]
          []
          []
        )
    _ -> FrozenInFear <$> runMessage msg attrs
