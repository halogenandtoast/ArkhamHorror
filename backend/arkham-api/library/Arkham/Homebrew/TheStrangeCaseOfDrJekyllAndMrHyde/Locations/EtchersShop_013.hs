{- | Etchers' Shop (013) -- buy a clue with an asset.

'DiscardAssetCost' already narrows to 'DiscardableAsset', so a permanent is not
offered up as payment.
-}
module Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Locations.EtchersShop_013 (etchersShop_013) where

import Arkham.Ability
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype EtchersShop_013 = EtchersShop_013 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

etchersShop_013 :: LocationCard EtchersShop_013
etchersShop_013 = location EtchersShop_013 Cards.etchersShop_013 4 (PerPlayer 2)

instance HasAbilities EtchersShop_013 where
  getAbilities (EtchersShop_013 a) =
    extendRevealed1 a
      $ scenarioI18n
      $ withI18nTooltip "etchersShop.discard"
      $ restricted a 1 (Here <> canDiscoverCluesAt (be a))
      $ actionAbilityWithCost (DiscardAssetCost $ AssetControlledBy You)

instance RunMessage EtchersShop_013 where
  runMessage msg l@(EtchersShop_013 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discoverAt NotInvestigate iid (attrs.ability 1) 1 attrs
      pure l
    _ -> EtchersShop_013 <$> liftRunMessage msg attrs
