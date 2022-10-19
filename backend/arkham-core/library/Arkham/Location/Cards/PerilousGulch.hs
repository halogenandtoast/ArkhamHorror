module Arkham.Location.Cards.PerilousGulch
  ( perilousGulch
  , PerilousGulch(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing

newtype PerilousGulch = PerilousGulch LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

perilousGulch :: LocationCard PerilousGulch
perilousGulch = location PerilousGulch Cards.perilousGulch 4 (PerPlayer 1)

instance HasAbilities PerilousGulch where
  getAbilities (PerilousGulch attrs) = withBaseAbilities
    attrs
    [ mkAbility attrs 1
      $ ForcedAbility
      $ SkillTestResult
          Timing.After
          You
          (WhileInvestigating $ LocationWithId $ toId attrs)
      $ FailureResult AnyValue
    ]

instance RunMessage PerilousGulch where
  runMessage msg l@(PerilousGulch attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      hasRope <-
        selectAny $ investigatorAt (toId attrs) <> InvestigatorWithSupply Rope
      unless hasRope $ push $ PlaceDoom (toTarget attrs) 1
      pure l
    _ -> PerilousGulch <$> runMessage msg attrs
