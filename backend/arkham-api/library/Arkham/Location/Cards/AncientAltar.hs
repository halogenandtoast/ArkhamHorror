module Arkham.Location.Cards.AncientAltar (ancientAltar) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose (chooseBeginSkillTest)
import Arkham.Scenarios.CourtOfTheAncients.Helpers (getVictoryGlyphCount)

newtype AncientAltar = AncientAltar LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancientAltar :: LocationCard AncientAltar
ancientAltar = location AncientAltar Cards.ancientAltar 4 (Static 2)

instance HasModifiersFor AncientAltar where
  getModifiersFor (AncientAltar a) = do
    -- Back: "Investigators cannot enter Ancient Altar unless there are 3 or more
    -- Glyph cards in the victory display." and "Entering Ancient Altar from East
    -- Antechamber costs 4 clues (as a group) per investigator."
    glyphs <- getVictoryGlyphCount
    modifySelect a (investigator_ $ at_ (be a))
      $ [CannotEnter (toId a) | glyphs < 3]
        <> [ AdditionalCostToEnterMatching
               (LocationWithTitle "East Antechamber")
               (GroupClueCost (PerPlayer 4) Anywhere)
           ]

instance HasAbilities AncientAltar where
  getAbilities (AncientAltar a) =
    extendRevealed1 a
      $ skillTestAbility
      $ restricted a 1 Here actionAbility

instance RunMessage AncientAltar where
  runMessage msg l@(AncientAltar attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      -- "Test [willpower] or [intellect] (5)" -- player chooses which skill.
      -- TODO: this test gets -1 difficulty for every 5 glyphs the investigators
      -- have translated (max -4). The translated-glyph count isn't readable yet
      -- (the campaign translateGlyph handler doesn't record a count), so we use a
      -- fixed difficulty of 5 and omit the per-5-translated reduction for now.
      chooseBeginSkillTest sid iid (attrs.ability 1) attrs [#willpower, #intellect] (Fixed 5)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      -- "take control of the set-aside Shard of Y'ch'lecht"
      shard <- getSetAsideCard Assets.shardOfYchlecht
      takeControlOfSetAsideAsset iid shard
      pure l
    _ -> AncientAltar <$> liftRunMessage msg attrs
