module Arkham.Scenarios.DevilReef.Helpers where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Campaigns.TheInnsmouthConspiracy.Memory
import Arkham.Helpers.Query
import Arkham.I18n
import Arkham.Id
import Arkham.Key
import Arkham.Message.Lifted
import Arkham.Prelude
import Arkham.Text

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "devilReef" a

noKeyAbilities :: [Ability] -> [Ability]
noKeyAbilities = filter \ab -> not (ab.index >= 500 && ab.index <= 520)

data Flashback = Flashback9

flashback :: ReverseQueue m => InvestigatorId -> Flashback -> m ()
flashback iid f = case f of
  Flashback9 -> do
    scenarioI18n $ story $ i18nWithTitle "flashback9"
    recoverMemory DiscoveryOfAStrangeIdol
    placeKey iid PurpleKey
    wavewornIdol <- getSetAsideCard Assets.wavewornIdol
    takeControlOfSetAsideAsset iid wavewornIdol
