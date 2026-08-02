module Arkham.Enemy.Cards.CourtKeeperObserverOfDreams (courtKeeperObserverOfDreams) where

import Arkham.Ability
import Arkham.Campaigns.TheDrownedCity.Import
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher

newtype CourtKeeperObserverOfDreams = CourtKeeperObserverOfDreams EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

courtKeeperObserverOfDreams :: EnemyCard CourtKeeperObserverOfDreams
courtKeeperObserverOfDreams = enemy CourtKeeperObserverOfDreams Cards.courtKeeperObserverOfDreams

instance HasModifiersFor CourtKeeperObserverOfDreams where
  getModifiersFor (CourtKeeperObserverOfDreams a) = do
    translatedGlyphs <- getTranslatedGlyphCount
    modifySelfWhen a (translatedGlyphs >= 10) [AddKeyword Keyword.Relentless]

instance HasAbilities CourtKeeperObserverOfDreams where
  getAbilities (CourtKeeperObserverOfDreams a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyDefeated #when You ByAny (be a)

instance RunMessage CourtKeeperObserverOfDreams where
  runMessage msg e@(CourtKeeperObserverOfDreams attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      -- You discover this glyph (rune_k). Record "Dreams" under rune_k; translated.
      campaignSpecific "translateGlyph" ("rune_k" :: Text, "Dreams" :: Text)
      pure e
    _ -> CourtKeeperObserverOfDreams <$> liftRunMessage msg attrs
