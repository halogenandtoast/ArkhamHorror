module Arkham.Investigator.Cards.DaisyWalker (daisyWalker) where

import Arkham.Action.Additional
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher
import Arkham.Script
import Arkham.Trait (Trait (Tome))

newtype DaisyWalker = DaisyWalker InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

daisyWalker :: InvestigatorCard DaisyWalker
daisyWalker =
  investigator DaisyWalker Cards.daisyWalker
    $ Stats {health = 5, sanity = 9, willpower = 3, intellect = 5, combat = 2, agility = 2}

instance HasChaosTokenValue DaisyWalker where
  getChaosTokenValue iid ElderSign (DaisyWalker attrs) | iid == attrs.id = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 0)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance HasModifiersFor DaisyWalker where
  getModifiersFor (DaisyWalker a) =
    modifySelf1 a
      $ GiveAdditionalAction
      $ AdditionalAction "Daisy Walker" (toSource a)
      $ TraitRestrictedAdditionalAction Tome AbilitiesOnly

instance RunMessage DaisyWalker where
  runMessage = script $ passedWithElderSign do
    drawCards you ElderSign `withCount` asset_ (yours <> #tome)
