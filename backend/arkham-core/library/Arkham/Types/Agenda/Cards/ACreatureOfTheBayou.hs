{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Agenda.Cards.ACreatureOfTheBayou where

import Arkham.Json
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Message
import ClassyPrelude hiding (sequence)
import Lens.Micro

newtype ACreatureOfTheBayou = ACreatureOfTheBayou Attrs
  deriving newtype (Show, ToJSON, FromJSON)

aCreatureOfTheBayou :: ACreatureOfTheBayou
aCreatureOfTheBayou = ACreatureOfTheBayou
  $ baseAttrs "81002" "A Creature of the Bayou" "Agenda 1a" (Static 5)

instance HasActions env investigator ACreatureOfTheBayou where
  getActions i window (ACreatureOfTheBayou x) = getActions i window x

instance (AgendaRunner env) => RunMessage env ACreatureOfTheBayou where
  runMessage msg (ACreatureOfTheBayou attrs@Attrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == "Agenda 1a" -> do
      pure
        $ ACreatureOfTheBayou
        $ attrs
        & sequence
        .~ "Agenda 1b"
        & flipped
        .~ True
    _ -> ACreatureOfTheBayou <$> runMessage msg attrs
