module Arkham.Scenarios.TheScarletKeys.ShadesOfSuffering.Helpers where

import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Keys
import Arkham.Campaigns.TheScarletKeys.Key.Matcher
import Arkham.Campaigns.TheScarletKeys.Key.Types
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import Arkham.I18n
import Arkham.Message.Lifted
import Arkham.Prelude
import Arkham.Projection
import Arkham.Source
import Arkham.Token

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "shadesOfSuffering" a

{- | The charges on The Shade Reaper, or 0 while the key is out of play.

The key leaves play with its bearer: when Tzu San Niang is defeated, or when the
investigator carrying it is eliminated. The scenario reference still prices
{skull} and resolves {cultist} after that (the skill-test window lists every
face's value, and the test that defeated her is still being applied), so a key
that is not in play simply holds no charges.
-}
getShadeReaperCharges :: HasGame m => m Int
getShadeReaperCharges =
  selectOne (scarletKeyIs Keys.theShadeReaper) >>= \case
    Nothing -> pure 0
    Just theShadeReaper -> fieldMap ScarletKeyTokens (countTokens #charge) theShadeReaper

-- | Place charges on The Shade Reaper. There is nothing to place them on once the key has left play.
placeShadeReaperCharges :: (ReverseQueue m, Sourceable source) => source -> Int -> m ()
placeShadeReaperCharges source n =
  whenJustM (selectOne $ scarletKeyIs Keys.theShadeReaper) \theShadeReaper ->
    placeTokens source theShadeReaper #charge n
