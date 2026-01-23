module Arkham.Location.Cards.IridescentPassage (iridescentPassage) where

import Arkham.Ability
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype IridescentPassage = IridescentPassage LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

iridescentPassage :: LocationCard IridescentPassage
iridescentPassage = locationWith IridescentPassage Cards.iridescentPassage 3 (PerPlayer 2) connectsToAdjacent

instance HasAbilities IridescentPassage where
  getAbilities (IridescentPassage a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> youExist (HandWith (HasCard DiscardableCard)))
      $ forced
      $ SkillTestResult #after You (WhileInvestigating $ be a) #failure

instance RunMessage IridescentPassage where
  runMessage msg l@(IridescentPassage attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseAndDiscardCard iid (attrs.ability 1)
      pure l
    _ -> IridescentPassage <$> liftRunMessage msg attrs
