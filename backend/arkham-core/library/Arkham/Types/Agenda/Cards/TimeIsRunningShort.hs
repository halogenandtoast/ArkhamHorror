{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Agenda.Cards.TimeIsRunningShort where

import Arkham.Import hiding (sequence)

import qualified Arkham.Types.Action as Action
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Helpers
import Arkham.Types.Agenda.Runner

newtype TimeIsRunningShort = TimeIsRunningShort Attrs
  deriving newtype (Show, ToJSON, FromJSON)

timeIsRunningShort :: TimeIsRunningShort
timeIsRunningShort = TimeIsRunningShort
  $ baseAttrs "01122" 2 "Time Is Running Short" "Agenda 2a" (Static 8)

instance HasModifiersFor env TimeIsRunningShort where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env TimeIsRunningShort where
  getActions iid NonFast (TimeIsRunningShort attrs) = do
    canAffordActions <- getCanAffordCost
      iid
      (toSource attrs)
      (ActionCost 1 (Just Action.Resign) mempty)
    pure
      [ ActivateCardAbilityAction
          iid
          (mkAbility
            (AgendaSource "01122")
            1
            (ActionAbility 1 (Just Action.Resign))
          )
      | canAffordActions
      ]
  getActions _ _ _ = pure []

instance (AgendaRunner env) => RunMessage env TimeIsRunningShort where
  runMessage msg (TimeIsRunningShort attrs@Attrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == "Agenda 2a" -> do
      leadInvestigatorId <- unLeadInvestigatorId <$> getId ()
      unshiftMessage (Ask leadInvestigatorId $ ChooseOne [Resolution 2])
      pure
        $ TimeIsRunningShort
        $ attrs
        & (sequence .~ "Agenda 2b")
        & (flipped .~ True)
    UseCardAbility iid (AgendaSource aid) _ 1 | aid == agendaId -> do
      unshiftMessage (Resign iid)
      TimeIsRunningShort <$> runMessage msg attrs
    _ -> TimeIsRunningShort <$> runMessage msg attrs
