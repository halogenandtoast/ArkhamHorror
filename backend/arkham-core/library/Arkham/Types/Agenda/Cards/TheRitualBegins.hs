{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Agenda.Cards.TheRitualBegins where

import Arkham.Json
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Trait
import ClassyPrelude hiding (sequence)
import qualified Data.HashSet as HashSet
import Lens.Micro

newtype TheRitualBegins = TheRitualBegins Attrs
  deriving newtype (Show, ToJSON, FromJSON)

theRitualBegins :: TheRitualBegins
theRitualBegins =
  TheRitualBegins $ baseAttrs "01144" "The Ritual Begins" "Agenda 2a" (Static 5)

instance HasActions env TheRitualBegins where
  getActions i window (TheRitualBegins x) = getActions i window x

instance (AgendaRunner env) => RunMessage env TheRitualBegins where
  runMessage msg a@(TheRitualBegins attrs@Attrs {..}) = case msg of
    NextAgenda _ aid | aid == agendaId && agendaSequence == "Agenda 2a" -> do
      enemyIds <- HashSet.toList <$> asks (getSet ())
      unshiftMessages
        $ [ AddModifiers
              (EnemyTarget eid)
              (AgendaSource agendaId)
              [EnemyFight 1, EnemyEvade 1]
          | eid <- enemyIds
          ]
      TheRitualBegins <$> runMessage msg attrs
    EnemySpawn _ eid -> a <$ unshiftMessage
      (AddModifiers
        (EnemyTarget eid)
        (AgendaSource agendaId)
        [EnemyFight 1, EnemyEvade 1]
      )
    AdvanceAgenda aid | aid == agendaId && agendaSequence == "Agenda 2a" -> do
      leadInvestigatorId <- unLeadInvestigatorId <$> asks (getId ())
      enemyIds <- HashSet.toList <$> asks (getSet ())
      unshiftMessages
        $ [ RemoveAllModifiersOnTargetFrom
              (EnemyTarget eid)
              (AgendaSource agendaId)
          | eid <- enemyIds
          ]
        <> [Ask leadInvestigatorId $ ChooseOne [AdvanceAgenda agendaId]]
      pure $ TheRitualBegins $ attrs & sequence .~ "Agenda 2b" & flipped .~ True
    AdvanceAgenda aid | aid == agendaId && agendaSequence == "Agenda 2b" -> do
      investigatorIds <- HashSet.toList <$> asks (getSet ())
      a <$ unshiftMessages
        ([ BeginSkillTest
             iid
             (AgendaSource agendaId)
             (InvestigatorTarget iid)
             Nothing
             SkillWillpower
             6
             []
             [ SearchCollectionForRandom
                 iid
                 (AgendaSource agendaId)
                 (PlayerTreacheryType, Just Madness)
             ]
             mempty
             mempty
         | iid <- investigatorIds
         ]
        <> [NextAgenda agendaId "01145"]
        )
    RequestedPlayerCard iid (AgendaSource aid) mcard | aid == agendaId -> do
      case mcard of
        Nothing -> pure a
        Just card -> a <$ unshiftMessages
          [ AddToHand iid (PlayerCard card)
          , DrewPlayerTreachery iid (pcCardCode card) (pcId card)
          ]
    _ -> TheRitualBegins <$> runMessage msg attrs
