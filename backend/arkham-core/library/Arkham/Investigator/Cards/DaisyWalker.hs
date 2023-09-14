module Arkham.Investigator.Cards.DaisyWalker (
  DaisyWalker (..),
  daisyWalker,
) where

import Arkham.Prelude

import Arkham.Action.Additional
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher

newtype DaisyWalker = DaisyWalker InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

daisyWalker :: InvestigatorCard DaisyWalker
daisyWalker =
  investigator DaisyWalker Cards.daisyWalker
    $ Stats {health = 5, sanity = 9, willpower = 3, intellect = 5, combat = 2, agility = 2}

instance HasChaosTokenValue DaisyWalker where
  getChaosTokenValue iid ElderSign (DaisyWalker attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 0)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance HasModifiersFor DaisyWalker where
  getModifiersFor target (DaisyWalker a) | isTarget a target = do
    pure $ toModifiers a [GiveAdditionalAction $ TraitRestrictedAdditionalAction Tome AbilitiesOnly]
  getModifiersFor _ _ = pure []

instance RunMessage DaisyWalker where
  runMessage msg i@(DaisyWalker attrs) = case msg of
    PassedSkillTest iid _ _ (ChaosTokenTarget (chaosTokenFace -> ElderSign)) _ _ | attrs `is` iid -> do
      tomeCount <- selectCount $ assetControlledBy attrs.id <> withTrait Tome
      when (tomeCount > 0) $ pushM $ drawCards iid ElderSign tomeCount
      pure i
    _ -> DaisyWalker <$> runMessage msg attrs
