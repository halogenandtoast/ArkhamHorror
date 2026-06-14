module Arkham.Enemy.Cards.CourtKeeperWeaverOfNightmares (courtKeeperWeaverOfNightmares) where

import Arkham.Ability
import Arkham.Campaigns.TheDrownedCity.Import
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Message.Lifted.Log (record)

newtype CourtKeeperWeaverOfNightmares = CourtKeeperWeaverOfNightmares EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

courtKeeperWeaverOfNightmares :: EnemyCard CourtKeeperWeaverOfNightmares
courtKeeperWeaverOfNightmares = enemy CourtKeeperWeaverOfNightmares Cards.courtKeeperWeaverOfNightmares (3, Static 4, 3) (1, 1)

instance HasModifiersFor CourtKeeperWeaverOfNightmares where
  getModifiersFor (CourtKeeperWeaverOfNightmares a) = do
    -- TODO: "If the investigators have translated 10 or more glyphs" — the campaign
    -- translateGlyph handler isn't implemented yet, so there is no readable
    -- translated-glyph count. Gate on it once that count exists.
    let translatedGlyphs = 0 :: Int
    modifySelfWhen
      a
      (translatedGlyphs >= 10)
      [AddKeyword Keyword.Alert, AddKeyword Keyword.Retaliate]

instance HasAbilities CourtKeeperWeaverOfNightmares where
  getAbilities (CourtKeeperWeaverOfNightmares a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyDefeated #when You ByAny (be a)

instance RunMessage CourtKeeperWeaverOfNightmares where
  runMessage msg e@(CourtKeeperWeaverOfNightmares attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      record TheInvestigatorsDiscoveredAnAlienLanguage
      campaignSpecific "translateGlyph" ("rune_j" :: Text, "Sleep" :: Text)
      pure e
    _ -> CourtKeeperWeaverOfNightmares <$> liftRunMessage msg attrs
