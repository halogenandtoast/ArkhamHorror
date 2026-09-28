module Arkham.Homebrew.CircusExMortis.Locations.HiddenDungeon (hiddenDungeon) where

import Arkham.Ability
import Arkham.Deck qualified as Deck
import Arkham.Helpers.Modifiers (ModifierType (..), modifiedWhen_)
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Helpers.SkillTest (getSkillTest)
import Arkham.Homebrew.CircusExMortis.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.CircusExMortis.CardDefs.Treacheries qualified as Treacheries
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Placement

newtype HiddenDungeon = HiddenDungeon LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hiddenDungeon :: LocationCard HiddenDungeon
hiddenDungeon = location HiddenDungeon Cards.hiddenDungeon 4 (PerPlayer 1)

instance HasModifiersFor HiddenDungeon where
  getModifiersFor (HiddenDungeon a) = when (a.clues == 0) do
    whenJustM getSkillTest \st -> do
      assets <- select $ AssetAttachedTo $ TargetIs (toTarget a)
      treacheries <- select $ TreacheryAttachedToLocation (be a)
      let attached = map toTarget assets <> map toTarget treacheries
      modifiedWhen_ a (st.target `elem` attached) st [Difficulty (-2)]

instance HasAbilities HiddenDungeon where
  getAbilities (HiddenDungeon a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ RevealLocation #after You (be a)

instance RunMessage HiddenDungeon where
  runMessage msg l@(HiddenDungeon attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      captives <- getSetAsideCardsMatching (cardIs Assets.terrifiedCaptives)
      for_ captives \captive -> createAssetAt_ captive (AttachedToLocation attrs.id)
      shuffleCardsIntoDeck Deck.EncounterDeck
        =<< getSetAsideCardsMatching (cardIs Treacheries.wildHysteria)
      pure l
    _ -> HiddenDungeon <$> liftRunMessage msg attrs
