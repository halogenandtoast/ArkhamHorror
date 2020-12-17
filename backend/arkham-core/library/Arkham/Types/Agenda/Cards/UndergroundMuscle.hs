{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Agenda.Cards.UndergroundMuscle
  ( UndergroundMuscle(..)
  , undergroundMuscle
  )
where

import Arkham.Import

import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.EncounterSet
import System.Random.Shuffle

newtype UndergroundMuscle = UndergroundMuscle Attrs
  deriving newtype (Show, ToJSON, FromJSON)

undergroundMuscle :: UndergroundMuscle
undergroundMuscle = UndergroundMuscle
  $ baseAttrs "02064" "Underground Muscle" (Agenda 2 A) (Static 3)

instance HasActions env UndergroundMuscle where
  getActions i window (UndergroundMuscle x) = getActions i window x

instance HasModifiersFor env UndergroundMuscle where
  getModifiersFor = noModifiersFor

instance AgendaRunner env => RunMessage env UndergroundMuscle where
  runMessage msg (UndergroundMuscle attrs@Attrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 2 A -> do
      leadInvestigatorId <- unLeadInvestigatorId <$> getId ()
      unshiftMessage $ Ask leadInvestigatorId (ChooseOne [AdvanceAgenda aid])
      pure
        $ UndergroundMuscle
        $ attrs
        & (sequenceL .~ Agenda 2 B)
        & (flippedL .~ True)
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 2 B -> do
      leadInvestigatorId <- unLeadInvestigatorId <$> getId ()
      (enemy : rest) <- shuffleM =<< gatherEncounterSet HideousAbominations
      strikingFear <- gatherEncounterSet StrikingFear
      laBellaLunaInvestigators <- getSetList (LocationName "La Bella Luna")
      unshiftMessage
        (chooseOne
          leadInvestigatorId
          [ Label
              "Continue"
              ([ CreateEnemyAtLocationNamed
                 (getCardCode enemy)
                 (LocationName "Clover Club Lounge")
               , ShuffleEncounterDiscardBackIn
               , ShuffleIntoEncounterDeck $ rest <> strikingFear
               ]
              <> [ MoveAction iid "02070" False
                 | iid <- laBellaLunaInvestigators
                 ]
              )
          ]
        )
      pure
        $ UndergroundMuscle
        $ attrs
        & sequenceL
        .~ Agenda 1 B
        & flippedL
        .~ True
    _ -> UndergroundMuscle <$> runMessage msg attrs
