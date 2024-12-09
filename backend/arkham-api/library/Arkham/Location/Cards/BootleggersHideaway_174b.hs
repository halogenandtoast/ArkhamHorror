module Arkham.Location.Cards.BootleggersHideaway_174b (
  bootleggersHideaway_174b,
  BootleggersHideaway_174b (..),
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

newtype BootleggersHideaway_174b = BootleggersHideaway_174b LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bootleggersHideaway_174b :: LocationCard BootleggersHideaway_174b
bootleggersHideaway_174b =
  locationWith
    BootleggersHideaway_174b
    Cards.bootleggersHideaway_174b
    4
    (PerPlayer 1)
    connectsToAdjacent

instance HasAbilities BootleggersHideaway_174b where
  getAbilities (BootleggersHideaway_174b a) =
    extendRevealed
      a
      [ restricted a 1 UnrevealedKeyIsSetAside $ forced $ RevealLocation #after Anyone (be a)
      , groupLimit PerGame
          $ restricted a 2 (youExist $ InvestigatorWithKey BlackKey <> can.gain.resources) actionAbility
      ]

instance RunMessage BootleggersHideaway_174b where
  runMessage msg l@(BootleggersHideaway_174b attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeUnrevealedKeyOn attrs
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      gainResourcesIfCan iid (attrs.ability 2) 5
      pure l
    _ -> BootleggersHideaway_174b <$> liftRunMessage msg attrs
