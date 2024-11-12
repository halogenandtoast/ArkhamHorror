module Arkham.Scenarios.IntoTheMaelstrom.Helpers where

import Arkham.Act.Cards qualified as Acts
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Campaigns.TheInnsmouthConspiracy.Memory
import Arkham.Classes.HasQueue (push)
import Arkham.Helpers.Query (getLead, getSetAsideCard)
import Arkham.I18n
import Arkham.Id
import Arkham.Message (Message (AddAct, SetActDeckCards))
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Prelude
import Arkham.Text

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "intoTheMaelstrom" a

data Flashback = Flashback14

flashback :: ReverseQueue m => InvestigatorId -> Flashback -> m ()
flashback _iid f = case f of
  Flashback14 -> do
    scenarioI18n $ story $ i18nWithTitle "flashback14"
    recoverMemory AStingingBetrayal
    actV2 <- getSetAsideCard Acts.cityOfTheDeepV2
    actV3 <- getSetAsideCard Acts.cityOfTheDeepV3
    lead <- getLead
    chooseOneM lead do
      labeled
        "Help Agent Harper complete her mission. Put the set-aside City of the Deep (v. II) into play next to the current act. It provides a new alternate objective."
        do
          push $ SetActDeckCards 2 [actV2]
          push $ AddAct 2 actV2
      labeled
        "Defy Agent Harper. Search each player’s hand, deck, discard pile, and all play areas for Elina Harper and remove her from the game. Put the set-aside City of the Deep (v. III) into play next to the current act. It provides a new alternate objective."
        do
          push $ SetActDeckCards 2 [actV3]
          push $ AddAct 2 actV3
