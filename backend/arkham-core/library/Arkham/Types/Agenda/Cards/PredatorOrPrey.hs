{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Agenda.Cards.PredatorOrPrey where

import Arkham.Json
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Classes
import Arkham.Types.InvestigatorId
import Arkham.Types.FastWindow
import Arkham.Types.GameValue
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Query
import ClassyPrelude hiding (sequence)
import qualified Data.HashSet as HashSet

newtype PredatorOrPrey = PredatorOrPrey Attrs
  deriving newtype (Show, ToJSON, FromJSON)

predatorOrPrey :: PredatorOrPrey
predatorOrPrey =
  PredatorOrPrey $ baseAttrs "01121" "Predator or Prey?" "Agenda 1a" (Static 6)

instance (ActionRunner env investigator) => HasActions env investigator PredatorOrPrey where
  getActions i NonFast (PredatorOrPrey _) = pure
    [ ActivateCardAbilityAction
        (getId () i)
        (mkAbility
          (AgendaSource "01121")
          1
          (ActionAbility 1 (Just Action.Resign))
        )
    ]
  getActions _ _ _ = pure []

instance (AgendaRunner env) => RunMessage env PredatorOrPrey where
  runMessage msg a@(PredatorOrPrey attrs@Attrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == "Agenda 1a" -> do
      investigatorIds <- HashSet.toList <$> asks (getSet @InvestigatorId ())
      investigatorIdsWithClues <- asks $ \env ->
        for investigatorIds $ \iid -> (iid,) . unClueCount $ getCount @ClueCount iid env
      void $ error "Need to spawn the masked hunter"
      pure a
    UseCardAbility iid _ (AgendaSource aid) 1 | aid == agendaId -> do
      unshiftMessage (Resign iid)
      PredatorOrPrey <$> runMessage msg attrs
    _ -> PredatorOrPrey <$> runMessage msg attrs
