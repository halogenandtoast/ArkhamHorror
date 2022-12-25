module Arkham.Location.Cards.Infirmary
  ( infirmary
  , Infirmary(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Helpers.Investigator
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Message

newtype Infirmary = Infirmary LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

infirmary :: LocationCard Infirmary
infirmary = location Infirmary Cards.infirmary 3 (PerPlayer 1)

instance HasAbilities Infirmary where
  getAbilities (Infirmary attrs) = withBaseAbilities
    attrs
    [ limitedAbility (PlayerLimit PerRound 1)
        $ restrictedAbility attrs 1 Here (ActionAbility Nothing $ ActionCost 1)
    | locationRevealed attrs
    ]

instance RunMessage Infirmary where
  runMessage msg l@(Infirmary attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      healHorror <- canHaveHorrorHealed iid
      healDamage <- canHaveDamageHealed iid

      push $ chooseOne
        iid
        [ Label "Heal 1 damage and take 1 direct horror"
        $ [ HealDamage (toTarget attrs) (toSource attrs) 1 | healDamage ]
        <> [InvestigatorDirectDamage iid (toSource attrs) 0 1]
        , Label "Heal 1 horror and take 1 direct damage"
        $ [ HealHorror (toTarget attrs) (toSource attrs) 1 | healHorror ]
        <> [InvestigatorDirectDamage iid (toSource attrs) 1 0]
        ]
      pure l
    _ -> Infirmary <$> runMessage msg attrs
