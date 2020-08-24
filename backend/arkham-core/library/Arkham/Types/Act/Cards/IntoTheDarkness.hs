{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Act.Cards.IntoTheDarkness where

import Arkham.Json
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Source
import ClassyPrelude hiding (sequence)
import Lens.Micro

newtype IntoTheDarkness = IntoTheDarkness Attrs
  deriving newtype (Show, ToJSON, FromJSON)

intoTheDarkness :: IntoTheDarkness
intoTheDarkness =
  IntoTheDarkness $ baseAttrs "01147" "Into the Darkness" "Act 2a"

instance HasActions env investigator IntoTheDarkness where
  getActions i window (IntoTheDarkness x) = getActions i window x

instance (ActRunner env) => RunMessage env IntoTheDarkness where
  runMessage msg a@(IntoTheDarkness attrs@Attrs {..}) = case msg of
    AdvanceAct aid | aid == actId && actSequence == "Act 2a" -> do
      leadInvestigatorId <- unLeadInvestigatorId <$> asks (getId ())
      unshiftMessage (Ask leadInvestigatorId $ ChooseOne [AdvanceAct aid])
      pure $ IntoTheDarkness $ attrs & sequence .~ "Act 2b" & flipped .~ True
    AdvanceAct aid | aid == actId && actSequence == "Act 2b" -> do
      playerCount <- unPlayerCount <$> asks (getCount ())
      if playerCount > 3
        then a <$ unshiftMessages
          [ ShuffleEncounterDiscardBackIn
          , DiscardEncounterUntilFirst (ActSource actId) (EnemyType, Nothing)
          , DiscardEncounterUntilFirst (ActSource actId) (EnemyType, Nothing)
          , NextAct actId "01148"
          ]
        else a <$ unshiftMessages
          [ ShuffleEncounterDiscardBackIn
          , DiscardEncounterUntilFirst (ActSource actId) (EnemyType, Nothing)
          , NextAct actId "01148"
          ]
    RequestedEncounterCard (ActSource aid) mcard | aid == actId -> case mcard of
      Nothing -> pure a
      Just card ->
        a <$ unshiftMessages [SpawnEnemyAt (EncounterCard card) "01156"]
    WhenEnterLocation _ "01156" -> a <$ unshiftMessage (AdvanceAct actId)
    _ -> IntoTheDarkness <$> runMessage msg attrs
