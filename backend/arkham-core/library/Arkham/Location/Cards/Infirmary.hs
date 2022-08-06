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
    [ restrictedAbility attrs 1 Here (ActionAbility Nothing $ ActionCost 1)
      & abilityLimitL
      .~ PlayerLimit PerRound 1
    | locationRevealed attrs
    ]

instance RunMessage Infirmary where
  runMessage msg l@(Infirmary attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> l <$ push
      (chooseOne
        iid
        [ Label
          "Heal 1 damage and take 1 direct horror"
          [ HealDamage (toTarget attrs) 1
          , InvestigatorDirectDamage iid (toSource attrs) 0 1
          ]
        , Label
          "Heal 1 horror and take 1 direct damage"
          [ HealHorror (toTarget attrs) 1
          , InvestigatorDirectDamage iid (toSource attrs) 1 0
          ]
        ]
      )
    _ -> Infirmary <$> runMessage msg attrs
