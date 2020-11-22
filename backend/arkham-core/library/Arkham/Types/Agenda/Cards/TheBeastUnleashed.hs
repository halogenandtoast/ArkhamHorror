{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Agenda.Cards.TheBeastUnleashed where

import Arkham.Import hiding (sequence)

import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Game.Helpers
import Arkham.Types.LocationMatcher

newtype TheBeastUnleashed = TheBeastUnleashed Attrs
  deriving newtype (Show, ToJSON, FromJSON)

theBeastUnleashed :: TheBeastUnleashed
theBeastUnleashed = TheBeastUnleashed
  $ baseAttrs "02044" 3 "The Beast Unleashed" "Agenda 3a" (Static 2)

instance HasActions env TheBeastUnleashed where
  getActions i window (TheBeastUnleashed x) = getActions i window x

instance HasModifiersFor env TheBeastUnleashed where
  getModifiersFor = noModifiersFor

instance AgendaRunner env => RunMessage env TheBeastUnleashed where
  runMessage msg a@(TheBeastUnleashed attrs@Attrs {..}) = case msg of
    AdvanceAgendaIfThresholdSatisfied -> do
      perPlayerDoomThreshold <- getPlayerCountValue agendaDoomThreshold
      totalDoom <- unDoomCount <$> getCount ()
      experimentId <- unStoryEnemyId . fromJustNote "must be in play" <$> getId
        (CardCode "02058")
      a <$ when
        (totalDoom >= perPlayerDoomThreshold)
        (unshiftMessages
          [ RemoveAllDoom
          , MoveToward
            (EnemyTarget experimentId)
            (LocationNamed $ LocationName "Dormitories")
          ]
        )
    AdvanceAgenda aid | aid == agendaId && agendaSequence == "Agenda 2a" -> do
      investigatorIds <- getInvestigatorIds
      unshiftMessages
        $ [ InvestigatorAssignDamage iid (toSource attrs) 0 3
          | iid <- investigatorIds
          ]
        <> [Label "Resolution 3" [Resolution 3]]
      pure
        . TheBeastUnleashed
        $ attrs
        & sequence
        .~ "Agenda 2b"
        & flipped
        .~ True
    _ -> TheBeastUnleashed <$> runMessage msg attrs
