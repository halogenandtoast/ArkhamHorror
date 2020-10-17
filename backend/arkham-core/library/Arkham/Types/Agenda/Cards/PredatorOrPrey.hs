{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Agenda.Cards.PredatorOrPrey where

import Arkham.Import
import qualified Arkham.Types.Action as Action
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Helpers
import Arkham.Types.Agenda.Runner

newtype PredatorOrPrey = PredatorOrPrey Attrs
  deriving newtype (Show, ToJSON, FromJSON)

predatorOrPrey :: PredatorOrPrey
predatorOrPrey =
  PredatorOrPrey $ baseAttrs "01121" "Predator or Prey?" "Agenda 1a" (Static 6)

instance ActionRunner env  => HasActions env PredatorOrPrey where
  getActions iid NonFast (PredatorOrPrey _) = do
    hasActionsRemaining <- getHasActionsRemaining
      iid
      (Just Action.Resign)
      mempty
    pure
      [ ActivateCardAbilityAction
          iid
          (mkAbility
            (AgendaSource "01121")
            1
            (ActionAbility 1 (Just Action.Resign))
          )
      | hasActionsRemaining
      ]
  getActions _ _ _ = pure []

instance (AgendaRunner env) => RunMessage env PredatorOrPrey where
  runMessage msg a@(PredatorOrPrey attrs@Attrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == "Agenda 1a" ->
      a <$ unshiftMessages
        [CreateEnemyEngagedWithPrey "01121b", NextAgenda aid "01122"]
    UseCardAbility iid (AgendaSource aid) _ 1 | aid == agendaId -> do
      unshiftMessage (Resign iid)
      PredatorOrPrey <$> runMessage msg attrs
    _ -> PredatorOrPrey <$> runMessage msg attrs
