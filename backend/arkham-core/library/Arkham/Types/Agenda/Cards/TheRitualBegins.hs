{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Agenda.Cards.TheRitualBegins where

import Arkham.Import hiding (sequence)

import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Trait

newtype TheRitualBegins = TheRitualBegins Attrs
  deriving newtype (Show, ToJSON, FromJSON)

theRitualBegins :: TheRitualBegins
theRitualBegins = TheRitualBegins
  $ baseAttrs "01144" 2 "The Ritual Begins" "Agenda 2a" (Static 5)

instance HasModifiersFor env TheRitualBegins where
  getModifiersFor _ (EnemyTarget _) (TheRitualBegins attrs)
    | agendaSequence attrs == "Agenda 2a" = pure [EnemyFight 1, EnemyEvade 1]
  getModifiersFor _ _ _ = pure []

instance HasActions env TheRitualBegins where
  getActions i window (TheRitualBegins x) = getActions i window x

instance (AgendaRunner env) => RunMessage env TheRitualBegins where
  runMessage msg a@(TheRitualBegins attrs@Attrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == "Agenda 2a" -> do
      leadInvestigatorId <- unLeadInvestigatorId <$> asks (getId ())
      unshiftMessage (chooseOne leadInvestigatorId [AdvanceAgenda agendaId])
      pure $ TheRitualBegins $ attrs & sequence .~ "Agenda 2b" & flipped .~ True
    AdvanceAgenda aid | aid == agendaId && agendaSequence == "Agenda 2b" -> do
      investigatorIds <- asks $ setToList . getSet ()
      a <$ unshiftMessages
        ([ BeginSkillTest
             iid
             (AgendaSource agendaId)
             (InvestigatorTarget iid)
             Nothing
             SkillWillpower
             6
         | iid <- investigatorIds
         ]
        <> [NextAgenda agendaId "01145"]
        )
    FailedSkillTest iid _ source _ _ | isSource attrs source ->
      a <$ unshiftMessage
        (SearchCollectionForRandom
          iid
          (AgendaSource agendaId)
          (PlayerTreacheryType, Just Madness)
        )
    RequestedPlayerCard iid (AgendaSource aid) mcard | aid == agendaId -> do
      case mcard of
        Nothing -> pure a
        Just card -> a <$ unshiftMessages
          [ AddToHand iid (PlayerCard card)
          , DrewPlayerTreachery iid (pcCardCode card) (pcId card)
          ]
    _ -> TheRitualBegins <$> runMessage msg attrs
