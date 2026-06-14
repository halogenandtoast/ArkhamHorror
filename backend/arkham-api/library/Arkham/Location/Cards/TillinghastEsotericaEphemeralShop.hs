module Arkham.Location.Cards.TillinghastEsotericaEphemeralShop (tillinghastEsotericaEphemeralShop) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Trait (Trait (Artifact))

newtype TillinghastEsotericaEphemeralShop = TillinghastEsotericaEphemeralShop LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tillinghastEsotericaEphemeralShop :: LocationCard TillinghastEsotericaEphemeralShop
tillinghastEsotericaEphemeralShop = location TillinghastEsotericaEphemeralShop Cards.tillinghastEsotericaEphemeralShop 4 (Static 3)

instance HasAbilities TillinghastEsotericaEphemeralShop where
  getAbilities (TillinghastEsotericaEphemeralShop a) =
    extendRevealed1 a $ fastAbility a 1 Free Here

instance RunMessage TillinghastEsotericaEphemeralShop where
  runMessage msg l@(TillinghastEsotericaEphemeralShop attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      -- TODO: put this location into play adjacent to your location and connect
      -- it bidirectionally to the location it was drawn from. The adjacency/
      -- connection placement has no engine support yet, so it is handled at the
      -- placement layer / left best-effort here.
      randall <- getSetAsideCard Enemies.randallTillinghast
      createEnemyAt_ randall attrs
      -- Shuffle each set-aside Artifact asset and place them as a stack
      -- underneath this location.
      artifacts <- shuffle =<< getSetAsideCardsMatching (#asset <> CardWithTrait Artifact)
      placeUnderneath attrs artifacts
      pure l
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      placeCluesUpToClueValue (attrs.ability 1) attrs
      pure l
    _ -> TillinghastEsotericaEphemeralShop <$> liftRunMessage msg attrs
