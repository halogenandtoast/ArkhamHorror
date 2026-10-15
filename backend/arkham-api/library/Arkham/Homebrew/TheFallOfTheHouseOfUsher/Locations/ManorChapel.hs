module Arkham.Homebrew.TheFallOfTheHouseOfUsher.Locations.ManorChapel (manorChapel) where

import Arkham.Ability
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype ManorChapel = ManorChapel LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

manorChapel :: LocationCard ManorChapel
manorChapel = location ManorChapel Cards.manorChapel 3 (PerPlayer 2)

doomedAsset :: AssetMatcher
doomedAsset = AssetWithAnyDoom <> AssetControlledBy Anyone

instance HasAbilities ManorChapel where
  getAbilities (ManorChapel a) =
    extendRevealed
      a
      [ scenarioI18n
          $ withI18nTooltip "manorChapel.removeDoom"
          $ groupLimit PerRound
          $ restricted a 1 (Here <> exists doomedAsset) actionAbility
      , scenarioI18n $ collapseForced "manorChapel.collapse" a 2
      ]

instance RunMessage ManorChapel where
  runMessage msg l@(ManorChapel attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assets <- select doomedAsset
      chooseTargetM iid assets $ removeAllDoom (attrs.ability 1)
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      flipToRuined attrs
      pure l
    _ -> ManorChapel <$> liftRunMessage msg attrs
