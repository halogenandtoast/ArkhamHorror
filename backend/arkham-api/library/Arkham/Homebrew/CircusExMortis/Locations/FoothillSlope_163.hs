module Arkham.Homebrew.CircusExMortis.Locations.FoothillSlope_163 (foothillSlope_163) where

import Arkham.Helpers.History (hasHistory)
import Arkham.Helpers.Modifiers (ModifierType (..), modifyEach, modifySelf)
import Arkham.History.Types (HistoryType (RoundHistory))
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype FoothillSlope_163 = FoothillSlope_163 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

foothillSlope_163 :: LocationCard FoothillSlope_163
foothillSlope_163 = location FoothillSlope_163 Cards.foothillSlope_163 2 (PerPlayer 1)

instance HasModifiersFor FoothillSlope_163 where
  getModifiersFor (FoothillSlope_163 a) = whenRevealed a do
    -- caps a single discovery; the per-round half is the history check below
    modifySelf a [MaxCluesDiscovered 1]
    spent <- filterM (hasHistory RoundHistory $ CluesDiscoveredAt (atLeast 1) a.id) =<< select Anyone
    modifyEach a spent [CannotDiscoverCluesAt (be a)]

instance RunMessage FoothillSlope_163 where
  runMessage msg (FoothillSlope_163 attrs) = runQueueT $ FoothillSlope_163 <$> liftRunMessage msg attrs
