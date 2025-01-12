module Arkham.Investigator.Cards.SkidsOToole (skidsOToole) where

import Arkham.Ability hiding (you)
import Arkham.Script
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher hiding (DuringTurn)

newtype SkidsOToole = SkidsOToole InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

skidsOToole :: InvestigatorCard SkidsOToole
skidsOToole =
  investigator SkidsOToole Cards.skidsOToole
    $ Stats {health = 8, sanity = 6, willpower = 2, intellect = 3, combat = 3, agility = 4}

instance HasAbilities SkidsOToole where
  getAbilities (SkidsOToole a) =
    [playerLimit PerTurn $ restricted a 1 (Self <> DuringTurn You) $ FastAbility (ResourceCost 2)]

instance HasChaosTokenValue SkidsOToole where
  getChaosTokenValue iid ElderSign (SkidsOToole attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 2)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage SkidsOToole where
  runMessage = script do
    onAbility 1 $ gainActions you ability 1
    passedWithElderSign $ gainResources you ElderSign 2
