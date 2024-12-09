module Arkham.Location.Cards.LodgeCatacombs (lodgeCatacombs, LodgeCatacombs (..)) where

import Arkham.Ability
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Key
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype LodgeCatacombs = LodgeCatacombs LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lodgeCatacombs :: LocationCard LodgeCatacombs
lodgeCatacombs = location LodgeCatacombs Cards.lodgeCatacombs 4 (Static 0)

instance HasModifiersFor LodgeCatacombs where
  getModifiersFor (LodgeCatacombs a) = whenUnrevealed a do
    modifySelect
      a
      (not_ $ mapOneOf InvestigatorWithKey [ElderThingKey, SkullKey, CultistKey, TabletKey])
      [CannotEnter a.id]

instance HasAbilities LodgeCatacombs where
  getAbilities (LodgeCatacombs a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ RevealLocation #after You $ be a

instance RunMessage LodgeCatacombs where
  runMessage msg l@(LodgeCatacombs attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      sanctumDoorways <- shuffleM =<< getSetAsideCardsMatching (CardWithTitle "Sanctum Doorway")
      for_ (withIndex1 sanctumDoorways) \(idx, sanctumDoorway) -> do
        locationId <- placeLocation sanctumDoorway
        push $ SetLocationLabel locationId $ "sanctumDoorway" <> tshow idx
      whenNone (locationIs Cards.innerSanctum) do
        placeSetAsideLocation_ Cards.innerSanctum
      pure l
    _ -> LodgeCatacombs <$> liftRunMessage msg attrs
