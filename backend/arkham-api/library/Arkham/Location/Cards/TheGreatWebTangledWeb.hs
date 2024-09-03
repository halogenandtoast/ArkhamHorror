module Arkham.Location.Cards.TheGreatWebTangledWeb (
  theGreatWebTangledWeb,
  TheGreatWebTangledWeb (..),
)
where

import Arkham.Ability
import Arkham.Direction
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype TheGreatWebTangledWeb = TheGreatWebTangledWeb LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theGreatWebTangledWeb :: LocationCard TheGreatWebTangledWeb
theGreatWebTangledWeb =
  locationWith
    TheGreatWebTangledWeb
    Cards.theGreatWebTangledWeb
    2
    (PerPlayer 2)
    (connectsToL .~ setFromList [Above, Below])

instance HasAbilities TheGreatWebTangledWeb where
  getAbilities (TheGreatWebTangledWeb attrs) =
    extendRevealed
      attrs
      [ restrictedAbility attrs 1 (exists $ investigatorAt attrs) $ forced $ PhaseEnds #when #investigation
      ]

instance RunMessage TheGreatWebTangledWeb where
  runMessage msg l@(TheGreatWebTangledWeb attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeDoom (attrs.ability 1) attrs 1
      pure l
    _ -> TheGreatWebTangledWeb <$> liftRunMessage msg attrs
