module Arkham.Location.Cards.VenetianGarden
  ( venetianGarden
  , VenetianGarden(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Damage
import Arkham.Direction
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Target

newtype VenetianGarden = VenetianGarden LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

venetianGarden :: LocationCard VenetianGarden
venetianGarden = locationWith
  VenetianGarden
  Cards.venetianGarden
  3
  (PerPlayer 1)
  (connectsToL .~ singleton RightOf)

instance HasAbilities VenetianGarden where
  getAbilities (VenetianGarden attrs) =
    withBaseAbilities attrs
      $ [ limitedAbility (PlayerLimit PerGame 1)
          $ restrictedAbility
              attrs
              1
              (Here <> InvestigatorExists
                (HealableInvestigator (toSource attrs) HorrorType You)
              )
          $ ActionAbility Nothing
          $ Costs [ActionCost 2, ResourceCost 2]
        | locationRevealed attrs
        ]

instance RunMessage VenetianGarden where
  runMessage msg l@(VenetianGarden attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source ->
      l <$ push (HealHorror (InvestigatorTarget iid) (toSource attrs) 2)
    _ -> VenetianGarden <$> runMessage msg attrs
