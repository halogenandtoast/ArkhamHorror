module Arkham.Location.Cards.Parlor where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher

newtype Parlor = Parlor LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

parlor :: LocationCard Parlor
parlor = location Parlor Cards.parlor 2 (Static 0)

instance HasModifiersFor Parlor where
  getModifiersFor target (Parlor attrs) | attrs `is` target = do
    pure $ toModifiers attrs [Blocked | not (locationRevealed attrs)]
  getModifiersFor _ _ = pure []

instance HasAbilities Parlor where
  getAbilities (Parlor attrs) =
    withRevealedAbilities attrs
      $ [ withTooltip "\"This is too much for me!\" You run out the front door, fleeing in panic."
            $ locationResignAction attrs
        , restrictedAbility
            (proxied (assetIs Cards.litaChantler) attrs)
            1
            (Uncontrolled <> OnSameLocation)
            #parley
        ]

instance RunMessage Parlor where
  runMessage msg l@(Parlor attrs) = case msg of
    UseThisAbility iid (isProxySource attrs -> True) 1 -> do
      aid <- selectJust $ assetIs Cards.litaChantler
      sid <- getRandom
      push $ parley sid iid attrs aid #intellect (Fixed 4)
      pure l
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      aid <- selectJust $ assetIs Cards.litaChantler
      push $ TakeControlOfAsset iid aid
      pure l
    _ -> Parlor <$> runMessage msg attrs
