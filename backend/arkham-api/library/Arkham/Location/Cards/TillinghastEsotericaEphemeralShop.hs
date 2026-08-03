module Arkham.Location.Cards.TillinghastEsotericaEphemeralShop (tillinghastEsotericaEphemeralShop) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Location (getLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelf)
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (Field (LocationLabel))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Scenarios.TheDoomOfArkhamPartI.Helpers
import Arkham.Trait (Trait (Artifact))

newtype TillinghastEsotericaEphemeralShop = TillinghastEsotericaEphemeralShop LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tillinghastEsotericaEphemeralShop :: LocationCard TillinghastEsotericaEphemeralShop
tillinghastEsotericaEphemeralShop =
  location TillinghastEsotericaEphemeralShop Cards.tillinghastEsotericaEphemeralShop 4 (Static 3)

-- | The location this card was drawn from, recorded by its Revelation.
drawnFrom :: LocationAttrs -> Maybe LocationId
drawnFrom a = toResultDefault Nothing a.meta

{- | "Tillinghast Esoterica is connected to the location from which it was drawn, and
vice versa." The shop has no printed connection symbols of its own, so both directions
have to be granted; its door only ever opens onto the one location.
-}
instance HasModifiersFor TillinghastEsotericaEphemeralShop where
  getModifiersFor (TillinghastEsotericaEphemeralShop a) = for_ (drawnFrom a) \lid -> do
    modifySelf a [ConnectedToWhen (be a) (LocationWithId lid)]
    modifySelect a (LocationWithId lid) [ConnectedToWhen (LocationWithId lid) (be a)]

instance HasAbilities TillinghastEsotericaEphemeralShop where
  getAbilities (TillinghastEsotericaEphemeralShop a) =
    extendRevealed1 a $ restricted a 1 (exists $ be a <> LocationNotAtClueLimit) (FastAbility Free)

instance RunMessage TillinghastEsotericaEphemeralShop where
  runMessage msg l@(TillinghastEsotericaEphemeralShop attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      -- Drawing the card already put the location into play, so "put Tillinghast
      -- Esoterica into play adjacent to your location" comes down to which map slot
      -- it takes and what it connects to. It is only ever drawn from beneath the
      -- location the drawing investigator is standing on.
      randall <- getSetAsideCard Enemies.randallTillinghast
      createEnemyAt_ randall attrs
      -- "Shuffle each set-aside [[Artifact]] asset and place them underneath it as a
      -- stack." The Phantom Shop's action peels them off the top one at a time.
      artifacts <- shuffle =<< getSetAsideCardsMatching (#asset <> CardWithTrait Artifact)
      placeUnderneath attrs artifacts
      getLocationOf iid >>= \case
        Nothing -> pure l
        Just lid -> do
          fromLabel <- field LocationLabel lid
          pure
            $ TillinghastEsotericaEphemeralShop
            $ attrs
            & setMeta (Just lid)
            & labelL
            .~ esotericaSlot fromLabel
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      placeCluesUpToClueValue (attrs.ability 1) attrs
      pure l
    _ -> TillinghastEsotericaEphemeralShop <$> liftRunMessage msg attrs
