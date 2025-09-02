module Arkham.Location.Cards.RitualSiteTeetawn (ritualSiteTeetawn) where

import Arkham.Ability
import Arkham.Card
import Arkham.Helpers.FetchCard
import Arkham.Helpers.SkillTest.Lifted
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype RitualSiteTeetawn = RitualSiteTeetawn LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ritualSiteTeetawn :: LocationCard RitualSiteTeetawn
ritualSiteTeetawn = location RitualSiteTeetawn Cards.ritualSiteTeetawn 3 (PerPlayer 1)

instance HasAbilities RitualSiteTeetawn where
  getAbilities (RitualSiteTeetawn a) =
    extendRevealed1 a $ skillTestAbility $ restricted a 1 Here actionAbility

instance RunMessage RitualSiteTeetawn where
  runMessage msg l@(RitualSiteTeetawn attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      combinationSkillTest sid iid (attrs.ability 1) iid [#willpower, #agility] (Fixed 5)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      assets <- select $ DiscardableAsset <> assetControlledBy iid <> AssetNonStory
      cards <- select $ inHandOf NotForPlay iid <> basic #asset
      teetawnPassage <- selectJust $ locationIs Cards.teetawnPassage

      chooseUpToNM_ iid 2 do
        targets assets $ fetchCard >=> placeUnderneath teetawnPassage . only
        targets cards $ placeUnderneath teetawnPassage . only

      unless (null assets && null cards) do
        discardTopOfEncounterDeckAndHandle iid (attrs.ability 1) 1 attrs
      pure l
    DiscardedTopOfEncounterDeck iid cards _ (isTarget attrs -> True) -> do
      for_ cards \c -> when (cardMatch c $ card_ #enemy) $ drawCard iid c
      pure l
    _ -> RitualSiteTeetawn <$> liftRunMessage msg attrs
