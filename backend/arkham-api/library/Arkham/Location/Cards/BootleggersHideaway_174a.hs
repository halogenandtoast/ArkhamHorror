module Arkham.Location.Cards.BootleggersHideaway_174a (
  bootleggersHideaway_174a,
  BootleggersHideaway_174a (..),
)
where

import Arkham.Ability
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Capability
import Arkham.Key
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype BootleggersHideaway_174a = BootleggersHideaway_174a LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bootleggersHideaway_174a :: LocationCard BootleggersHideaway_174a
bootleggersHideaway_174a =
  locationWith
    BootleggersHideaway_174a
    Cards.bootleggersHideaway_174a
    4
    (PerPlayer 1)
    connectsToAdjacent

instance HasAbilities BootleggersHideaway_174a where
  getAbilities (BootleggersHideaway_174a a) =
    extendRevealed
      a
      [ restricted a 1 UnrevealedKeyIsSetAside $ forced $ RevealLocation #after Anyone (be a)
      , groupLimit PerGame
          $ restricted a 2 (youExist $ InvestigatorWithKey WhiteKey <> can.draw.cards) actionAbility
      ]

instance RunMessage BootleggersHideaway_174a where
  runMessage msg l@(BootleggersHideaway_174a attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeUnrevealedKeyOn attrs
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      drawCardsIfCan iid (attrs.ability 2) 3
      pure l
    _ -> BootleggersHideaway_174a <$> liftRunMessage msg attrs
