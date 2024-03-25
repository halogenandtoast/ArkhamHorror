module Arkham.Location.Cards.TheGreatWebWebStairs (
  theGreatWebWebStairs,
  TheGreatWebWebStairs (..),
)
where

import Arkham.Ability
import Arkham.Direction
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype TheGreatWebWebStairs = TheGreatWebWebStairs LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theGreatWebWebStairs :: LocationCard TheGreatWebWebStairs
theGreatWebWebStairs =
  locationWith
    TheGreatWebWebStairs
    Cards.theGreatWebWebStairs
    5
    (PerPlayer 1)
    (connectsToL .~ setFromList [Above, Below])

instance HasAbilities TheGreatWebWebStairs where
  getAbilities (TheGreatWebWebStairs attrs) =
    extendRevealed
      attrs
      [forcedAbility attrs 1 $ RevealLocation #after Anyone $ be attrs]

instance RunMessage TheGreatWebWebStairs where
  runMessage msg l@(TheGreatWebWebStairs attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      placeTokens (attrs.ability 1) attrs #doom 1
      pure l
    _ -> TheGreatWebWebStairs <$> lift (runMessage msg attrs)
