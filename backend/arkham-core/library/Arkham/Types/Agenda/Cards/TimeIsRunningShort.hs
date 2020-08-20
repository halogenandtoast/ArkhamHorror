{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Agenda.Cards.TimeIsRunningShort where

import Arkham.Json
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Classes
import Arkham.Types.FastWindow
import Arkham.Types.GameValue
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Source
import ClassyPrelude hiding (sequence)
import Lens.Micro

newtype TimeIsRunningShort = TimeIsRunningShort Attrs
  deriving newtype (Show, ToJSON, FromJSON)

timeIsRunningShort :: TimeIsRunningShort
timeIsRunningShort = TimeIsRunningShort
  $ baseAttrs "01122" "Time Is Running Short" "Agenda 2a" (Static 8)

instance (ActionRunner env investigator) => HasActions env investigator TimeIsRunningShort where
  getActions i NonFast (TimeIsRunningShort _) = pure
    [ ActivateCardAbilityAction
        (getId () i)
        (mkAbility
          (AgendaSource "01121")
          1
          (ActionAbility 1 (Just Action.Resign))
        )
    ]
  getActions _ _ _ = pure []

instance (AgendaRunner env) => RunMessage env TimeIsRunningShort where
  runMessage msg (TimeIsRunningShort attrs@Attrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == "Agenda 2a" -> do
      leadInvestigatorId <- unLeadInvestigatorId <$> asks (getId ())
      unshiftMessage (Ask leadInvestigatorId $ ChooseOne [Resolution 2])
      pure
        $ TimeIsRunningShort
        $ attrs
        & (sequence .~ "Agenda 2b")
        & (flipped .~ True)
    UseCardAbility iid _ (AgendaSource aid) 1 | aid == agendaId -> do
      unshiftMessage (Resign iid)
      TimeIsRunningShort <$> runMessage msg attrs
    _ -> TimeIsRunningShort <$> runMessage msg attrs
