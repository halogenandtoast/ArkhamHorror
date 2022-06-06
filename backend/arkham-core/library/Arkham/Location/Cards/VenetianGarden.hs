module Arkham.Location.Cards.VenetianGarden
  ( venetianGarden
  , VenetianGarden(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Direction
import Arkham.GameValue
import Arkham.Location.Runner
import Arkham.Location.Helpers
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
  NoSymbol
  []
  (connectsToL .~ singleton RightOf)

instance HasAbilities VenetianGarden where
  getAbilities (VenetianGarden attrs) =
    withBaseAbilities attrs $
      [ restrictedAbility
            attrs
            1
            Here
            (ActionAbility Nothing $ Costs [ActionCost 2, ResourceCost 2])
          & (abilityLimitL .~ PlayerLimit PerGame 1)
      | locationRevealed attrs
      ]

instance LocationRunner env => RunMessage VenetianGarden where
  runMessage msg l@(VenetianGarden attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ push (HealHorror (InvestigatorTarget iid) 2)
    _ -> VenetianGarden <$> runMessage msg attrs
