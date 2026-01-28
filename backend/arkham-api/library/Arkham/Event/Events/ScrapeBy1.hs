module Arkham.Event.Events.ScrapeBy1 (scrapeBy1) where

import Arkham.ChaosToken.Types (isSymbolChaosToken)
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.SkillTest (getSkillTestRevealedChaosTokens)

newtype ScrapeBy1 = ScrapeBy1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scrapeBy1 :: EventCard ScrapeBy1
scrapeBy1 = event ScrapeBy1 Cards.scrapeBy1

instance RunMessage ScrapeBy1 where
  runMessage msg e@(ScrapeBy1 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      tokens <- getSkillTestRevealedChaosTokens
      when (any (isSymbolChaosToken . (.face)) tokens) do
        assignHorror iid attrs 1
      passSkillTest
      pure e
    _ -> ScrapeBy1 <$> liftRunMessage msg attrs
