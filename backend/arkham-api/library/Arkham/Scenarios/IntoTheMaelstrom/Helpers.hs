module Arkham.Scenarios.IntoTheMaelstrom.Helpers where

import Arkham.Act.Cards qualified as Acts
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Campaigns.TheInnsmouthConspiracy.Memory
import Arkham.Classes.HasQueue (push)
import Arkham.Classes.Query (select)
import Arkham.Helpers.Query (getLead, getSetAsideCard)
import Arkham.I18n
import Arkham.Id
import Arkham.Matcher
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
    chooseOneM lead $ scenarioI18n $ scope "flashback14" do
      labeled' "helpAgentHarper"
        do
          push $ SetActDeckCards 2 [actV2]
          push $ AddAct 2 actV2
      labeled' "defyAgentHarper"
        do
          selectEach (assetIs Assets.elinaHarperKnowsTooMuch) removeFromGame
          harperCards <-
            select
              $ basic (cardIs Assets.elinaHarperKnowsTooMuch)
              <> oneOf [InHandOf NotForPlay Anyone, InDeckOf Anyone, InDiscardOf Anyone]
          for_ harperCards removeCardFromGame
          push $ SetActDeckCards 2 [actV3]
          push $ AddAct 2 actV3
