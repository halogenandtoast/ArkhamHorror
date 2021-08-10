module Arkham.Types.Act.Cards.IntoTheDarkness where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Source

newtype IntoTheDarkness = IntoTheDarkness ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasActions, HasModifiersFor env)

intoTheDarkness :: ActCard IntoTheDarkness
intoTheDarkness = act (2, A) IntoTheDarkness Cards.intoTheDarkness Nothing

instance ActRunner env => RunMessage env IntoTheDarkness where
  runMessage msg a@(IntoTheDarkness attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide A attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      push (chooseOne leadInvestigatorId [AdvanceAct aid (toSource attrs)])
      pure $ IntoTheDarkness $ attrs & (sequenceL .~ Act 2 B)
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
      playerCount <- getPlayerCount
      if playerCount > 3
        then a <$ pushAll
          [ ShuffleEncounterDiscardBackIn
          , DiscardEncounterUntilFirst
            (ActSource actId)
            (CardWithType EnemyType)
          , DiscardEncounterUntilFirst
            (ActSource actId)
            (CardWithType EnemyType)
          , NextAct actId "01148"
          ]
        else a <$ pushAll
          [ ShuffleEncounterDiscardBackIn
          , DiscardEncounterUntilFirst
            (ActSource actId)
            (CardWithType EnemyType)
          , NextAct actId "01148"
          ]
    RequestedEncounterCard (ActSource aid) mcard | aid == actId -> case mcard of
      Nothing -> pure a
      Just card -> do
        ritualSiteId <- getJustLocationIdByName "Ritual Site"
        a <$ pushAll [SpawnEnemyAt (EncounterCard card) ritualSiteId]
    WhenEnterLocation _ lid -> do
      mRitualSiteId <- getLocationIdByName "Ritual Site"
      a <$ when
        (mRitualSiteId == Just lid)
        (push $ AdvanceAct actId (toSource attrs))
    _ -> IntoTheDarkness <$> runMessage msg attrs
