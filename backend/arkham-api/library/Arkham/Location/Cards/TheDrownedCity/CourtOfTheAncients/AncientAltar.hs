module Arkham.Location.Cards.TheDrownedCity.CourtOfTheAncients.AncientAltar (ancientAltar) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheDrownedCity.Helpers (getTranslatedGlyphCount)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Location.CardDefs.TheDrownedCity.CourtOfTheAncients qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose (chooseBeginSkillTest)
import Arkham.Scenarios.TheDrownedCity.CourtOfTheAncients.Helpers (getVictoryGlyphCount)

newtype AncientAltar = AncientAltar LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancientAltar :: LocationCard AncientAltar
ancientAltar = location AncientAltar Cards.ancientAltar 4 (PerPlayer 2)

instance HasModifiersFor AncientAltar where
  getModifiersFor (AncientAltar a) = do
    -- Back: "Investigators cannot enter Ancient Altar unless there are 3 or more
    -- Glyph cards in the victory display." and "Entering Ancient Altar from East
    -- Antechamber costs 4 clues (as a group) per investigator."
    --
    -- Both modifiers belong on the investigators who might *enter* the Altar, not
    -- on the ones standing on it: a CannotEnter/AdditionalCostToEnterMatching held
    -- by someone already here never gates the move in.
    glyphs <- getVictoryGlyphCount
    when (glyphs < 3) $ modifySelect a Anyone [CannotEnter (toId a)]
    -- East Antechamber is the Altar's only connection, so the toll is scoped to
    -- the investigators standing there.
    modifySelect
      a
      (investigator_ $ at_ (locationIs Cards.eastAntechamber))
      [AdditionalCostToEnterMatching (be a) (GroupClueCost (PerPlayer 4) Anywhere)]

instance HasAbilities AncientAltar where
  getAbilities (AncientAltar a) =
    extendRevealed1 a
      $ skillTestAbility
      $ restricted a 1 Here actionAbility

instance RunMessage AncientAltar where
  runMessage msg l@(AncientAltar attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      -- "Test [willpower] or [intellect] (5)" -- player chooses which skill. The
      -- test gets -1 difficulty for every 5 glyphs translated, to a max of -4.
      translated <- getTranslatedGlyphCount
      let difficulty = 5 - min 4 (translated `div` 5)
      chooseBeginSkillTest sid iid (attrs.ability 1) attrs [#willpower, #intellect] (Fixed difficulty)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      -- "take control of the set-aside Shard of Y'ch'lecht"
      shard <- getSetAsideCard Assets.shardOfYchlecht
      takeControlOfSetAsideAsset iid shard
      pure l
    _ -> AncientAltar <$> liftRunMessage msg attrs
