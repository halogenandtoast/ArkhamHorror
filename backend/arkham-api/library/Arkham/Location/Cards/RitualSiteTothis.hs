module Arkham.Location.Cards.RitualSiteTothis (ritualSiteTothis) where

import Arkham.Ability
import Arkham.Asset.Uses
import Arkham.Card
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype RitualSiteTothis = RitualSiteTothis LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ritualSiteTothis :: LocationCard RitualSiteTothis
ritualSiteTothis = location RitualSiteTothis Cards.ritualSiteTothis 3 (PerPlayer 1)

instance HasAbilities RitualSiteTothis where
  getAbilities (RitualSiteTothis a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> exists (not_ (LocationWithToken Shard) <> locationIs Cards.tothisBarrens))
      $ FastAbility Free

instance RunMessage RitualSiteTothis where
  runMessage msg l@(RitualSiteTothis attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discardTopOfEncounterDeckAndHandle iid (attrs.ability 1) 1 attrs
      tothisBarrens <- selectJust $ locationIs Cards.tothisBarrens
      placeTokens (attrs.ability 1) tothisBarrens Shard 1
      pure l
    DiscardedTopOfEncounterDeck _ cards _ (isTarget attrs -> True) -> do
      tothisBarrens <- selectJust $ locationIs Cards.tothisBarrens
      for_ cards \c -> when (cardMatch c $ card_ #enemy) $ spawnEnemyAt_ c tothisBarrens
      pure l
    _ -> RitualSiteTothis <$> liftRunMessage msg attrs
