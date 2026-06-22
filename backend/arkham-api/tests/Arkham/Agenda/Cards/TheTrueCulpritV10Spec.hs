module Arkham.Agenda.Cards.TheTrueCulpritV10Spec (spec) where

import Arkham.Agenda.Cards qualified as Agendas
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Modifier
import TestImport.New

-- | Regression for #4885. In Murder at the Excelsior Hotel the final agenda
-- "The True Culprit (v. X)" must enter play already holding its starting doom.
-- Its objective is a forced ability that is valid whenever the agenda has 0
-- doom, and the EnterPlay window fires as the agenda is created -- so if the
-- doom is placed afterwards the objective is hit first. We attach the doom via
-- an EntersPlayWithDoom modifier (as the act now does) and AddAgenda bakes it in
-- at construction, before the EnterPlay window.
spec :: Spec
spec = describe "The True Culprit (v. X)" do
  it "enters play already holding the doom from EntersPlayWithDoom" . gameTest $ \_self -> do
    card <- genCard Agendas.theTrueCulpritV10
    let aid = AgendaId (toCardCode card)
    run =<< gameModifier (TestSource mempty) (toTarget aid) (EntersPlayWithDoom 3)
    pushAndRun $ AddAgenda 1 card
    assertAny $ AgendaWithId aid <> AgendaWithDoom (EqualTo $ Static 3)
