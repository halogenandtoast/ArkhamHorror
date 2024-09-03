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
  deriving stock (Data)

daisyWalker :: InvestigatorCard DaisyWalker
daisyWalker =
  investigator DaisyWalker Cards.daisyWalker
    $ Stats {health = 5, sanity = 9, willpower = 3, intellect = 5, combat = 2, agility = 2}

instance HasChaosTokenValue DaisyWalker where
  getChaosTokenValue iid ElderSign (DaisyWalker attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 0)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance HasModifiersFor DaisyWalker where
  getModifiersFor target (DaisyWalker a) | a `is` target = do
    pure
      $ toModifiers
        a
        [ GiveAdditionalAction
            $ AdditionalAction "Daisy Walker" (toSource a)
            $ TraitRestrictedAdditionalAction Tome AbilitiesOnly
        ]
  getModifiersFor _ _ = pure []

instance RunMessage DaisyWalker where
  runMessage msg i@(DaisyWalker attrs) = case msg of
    PassedSkillTestWithToken iid ElderSign | attrs `is` iid -> do
      tomeCount <- selectCount $ assetControlledBy attrs.id <> withTrait Tome
      pushWhen (tomeCount > 0) $ drawCards iid ElderSign tomeCount
      pure i
    _ -> DaisyWalker <$> runMessage msg attrs
