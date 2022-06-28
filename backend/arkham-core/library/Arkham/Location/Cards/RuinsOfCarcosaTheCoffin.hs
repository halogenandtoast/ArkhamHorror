module Arkham.Location.Cards.RuinsOfCarcosaTheCoffin
  ( ruinsOfCarcosaTheCoffin
  , RuinsOfCarcosaTheCoffin(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing

newtype RuinsOfCarcosaTheCoffin = RuinsOfCarcosaTheCoffin LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ruinsOfCarcosaTheCoffin :: LocationCard RuinsOfCarcosaTheCoffin
ruinsOfCarcosaTheCoffin = locationWith
  RuinsOfCarcosaTheCoffin
  Cards.ruinsOfCarcosaTheCoffin
  2
  (PerPlayer 1)
  Triangle
  [Square, Equals, Star]
  (canBeFlippedL .~ True)

instance HasAbilities RuinsOfCarcosaTheCoffin where
  getAbilities (RuinsOfCarcosaTheCoffin a) = withBaseAbilities
    a
    [ mkAbility a 1 $ ForcedAbility $ DiscoveringLastClue
        Timing.After
        You
        (LocationWithId $ toId a)
    ]

instance RunMessage RuinsOfCarcosaTheCoffin where
  runMessage msg l@(RuinsOfCarcosaTheCoffin attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      push $ InvestigatorAssignDamage iid source DamageAny 1 0
      pure l
    _ -> RuinsOfCarcosaTheCoffin <$> runMessage msg attrs
