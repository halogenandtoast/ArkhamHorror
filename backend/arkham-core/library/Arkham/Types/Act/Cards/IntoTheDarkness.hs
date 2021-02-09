module Arkham.Types.Act.Cards.IntoTheDarkness where


import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner
import Arkham.Types.Card.EncounterCardMatcher

newtype IntoTheDarkness = IntoTheDarkness ActAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

intoTheDarkness :: IntoTheDarkness
intoTheDarkness =
  IntoTheDarkness $ baseAttrs "01147" "Into the Darkness" (Act 2 A) Nothing

instance ActionRunner env => HasActions env IntoTheDarkness where
  getActions i window (IntoTheDarkness x) = getActions i window x

instance ActRunner env => RunMessage env IntoTheDarkness where
  runMessage msg a@(IntoTheDarkness attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide A attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      unshiftMessage
        (chooseOne leadInvestigatorId [AdvanceAct aid (toSource attrs)])
      pure $ IntoTheDarkness $ attrs & (sequenceL .~ Act 2 B)
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
      playerCount <- getPlayerCount
      if playerCount > 3
        then a <$ unshiftMessages
          [ ShuffleEncounterDiscardBackIn
          , DiscardEncounterUntilFirst
            (ActSource actId)
            (EncounterCardMatchByType (EnemyType, Nothing))
          , DiscardEncounterUntilFirst
            (ActSource actId)
            (EncounterCardMatchByType (EnemyType, Nothing))
          , NextAct actId "01148"
          ]
        else a <$ unshiftMessages
          [ ShuffleEncounterDiscardBackIn
          , DiscardEncounterUntilFirst
            (ActSource actId)
            (EncounterCardMatchByType (EnemyType, Nothing))
          , NextAct actId "01148"
          ]
    RequestedEncounterCard (ActSource aid) mcard | aid == actId -> case mcard of
      Nothing -> pure a
      Just card ->
        a <$ unshiftMessages [SpawnEnemyAt (EncounterCard card) "01156"]
    WhenEnterLocation _ "01156" ->
      a <$ unshiftMessage (AdvanceAct actId (toSource attrs))
    _ -> IntoTheDarkness <$> runMessage msg attrs
