module Arkham.Treachery.Cards.CosmicOmen (cosmicOmen) where

import Arkham.Helpers.Message.Discard.Lifted (randomDiscard)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.CourtOfTheAncients.Helpers
import Arkham.Trait (Trait (Glyph))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype CosmicOmen = CosmicOmen TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cosmicOmen :: TreacheryCard CosmicOmen
cosmicOmen = treachery CosmicOmen Cards.cosmicOmen

instance RunMessage CosmicOmen where
  runMessage msg t@(CosmicOmen attrs) = runQueueT $ scenarioI18n $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      -- TODO: if the investigators have translated 10+ glyphs this test gets +2
      -- difficulty. The translated-glyph count is not currently readable, so the
      -- base difficulty (Fixed 3) is used without the conditional +2.
      revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      glyphs <- select $ TreacheryWithTrait Glyph <> InPlayTreachery
      chooseOrRunOneM iid $ scope "cosmicOmen" do
        when (notNull glyphs) do
          labeled' "shuffleGlyphIntoEncounterDeck" do
            chooseTargetM iid glyphs shuffleBackIntoEncounterDeck
        labeled' "takeHorrorAndDiscard" do
          directHorror iid attrs 1
          randomDiscard iid attrs
      pure t
    _ -> CosmicOmen <$> liftRunMessage msg attrs
