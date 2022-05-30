module Arkham.Act.Cards.LeadingTheWay
  ( LeadingTheWay(..)
  , leadingTheWay
  ) where

import Arkham.Prelude

import Arkham.Act.Attrs
import Arkham.Act.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Matcher
import Arkham.Ability
import Arkham.Criteria

newtype LeadingTheWay = LeadingTheWay ActAttrs
  deriving anyclass (IsAct, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

leadingTheWay :: ActCard LeadingTheWay
leadingTheWay = act (3, A) LeadingTheWay Cards.leadingTheWay Nothing

instance HasAbilities LeadingTheWay where
  getAbilities (LeadingTheWay a) =
    [ restrictedAbility
          a
          1
          (EachUndefeatedInvestigator $ InvestigatorAt $ locationIs
            Locations.blockedPassage
          )
        $ Objective
        $ ForcedAbility AnyWindow
    ]

instance ActRunner env => RunMessage env LeadingTheWay where
  runMessage msg (LeadingTheWay attrs) = LeadingTheWay <$> runMessage msg attrs
