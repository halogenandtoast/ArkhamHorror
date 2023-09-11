module Arkham.Location.Cards.Parlor where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher

newtype Parlor = Parlor LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

parlor :: LocationCard Parlor
parlor = location Parlor Cards.parlor 2 (Static 0)

instance HasModifiersFor Parlor where
  getModifiersFor target (Parlor attrs) | isTarget attrs target = do
    pure $ toModifiers attrs [Blocked | not (locationRevealed attrs)]
  getModifiersFor _ _ = pure []

instance HasAbilities Parlor where
  getAbilities (Parlor attrs) =
    withRevealedAbilities attrs
      $ [ withTooltip "\"This is too much for me!\" You run out the front door, fleeing in panic."
            $ locationResignAction attrs
        , restrictedAbility
            ( ProxySource
                (AssetMatcherSource $ assetIs Cards.litaChantler)
                (toSource attrs)
            )
            1
            (Uncontrolled <> OnSameLocation)
            $ ActionAbility (Just Action.Parley) (ActionCost 1)
        ]

instance RunMessage Parlor where
  runMessage msg l@(Parlor attrs) = case msg of
    UseCardAbility iid (ProxySource _ source) 1 _ _ | isSource attrs source -> do
      aid <- selectJust $ assetIs Cards.litaChantler
      push $ parley iid source aid #intellect 4
      pure l
    PassedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ _ -> do
      aid <- selectJust $ assetIs Cards.litaChantler
      push $ TakeControlOfAsset iid aid
      pure l
    _ -> Parlor <$> runMessage msg attrs
