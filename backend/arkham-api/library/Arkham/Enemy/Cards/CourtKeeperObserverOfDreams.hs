module Arkham.Enemy.Cards.CourtKeeperObserverOfDreams (courtKeeperObserverOfDreams) where

import Arkham.Ability
import Arkham.Campaigns.TheDrownedCity.Import
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Log (record)

newtype CourtKeeperObserverOfDreams = CourtKeeperObserverOfDreams EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

courtKeeperObserverOfDreams :: EnemyCard CourtKeeperObserverOfDreams
courtKeeperObserverOfDreams = enemy CourtKeeperObserverOfDreams Cards.courtKeeperObserverOfDreams (3, Static 4, 3) (1, 1)

-- TODO: "If the investigators have translated 10+ glyphs, this enemy gains
-- relentless." The translated-glyph count isn't readable yet (the campaign
-- translateGlyph handler doesn't record a count), so the Relentless keyword
-- cannot be conditionally granted here. Add this via `modifySelfWhen a
-- (translated >= 10) [AddKeyword Keyword.Relentless]` once that count exists.

instance HasAbilities CourtKeeperObserverOfDreams where
  getAbilities (CourtKeeperObserverOfDreams a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyDefeated #when You ByAny (be a)

instance RunMessage CourtKeeperObserverOfDreams where
  runMessage msg e@(CourtKeeperObserverOfDreams attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      -- You discover this glyph (rune_k). Record "Dreams" under rune_k; translated.
      record TheInvestigatorsDiscoveredAnAlienLanguage
      campaignSpecific "translateGlyph" ("rune_k" :: Text, "Dreams" :: Text)
      pure e
    _ -> CourtKeeperObserverOfDreams <$> liftRunMessage msg attrs
