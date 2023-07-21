module Arkham.Location.Cards.Balcony (
  balcony,
  Balcony (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype Balcony = Balcony LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

balcony :: LocationCard Balcony
balcony = location Balcony Cards.balcony 2 (PerPlayer 1)

instance HasAbilities Balcony where
  getAbilities (Balcony attrs) =
    withRevealedAbilities
      attrs
      [ mkAbility attrs 1 $
          ForcedAbility $
            MoveAction
              Timing.After
              You
              (LocationWithId $ toId attrs)
              (locationIs Cards.theatre)
      ]

instance RunMessage Balcony where
  runMessage msg l@(Balcony attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ InvestigatorAssignDamage iid (toSource attrs) DamageAny 2 0
      pure l
    _ -> Balcony <$> runMessage msg attrs
