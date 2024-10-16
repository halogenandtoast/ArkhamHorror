module Arkham.Location.Cards.SawboneAlleyInTooDeep (sawboneAlleyInTooDeep, SawboneAlleyInTooDeep (..)) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers (connectsToAdjacent)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.InTooDeep.Helpers

newtype SawboneAlleyInTooDeep = SawboneAlleyInTooDeep LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sawboneAlleyInTooDeep :: LocationCard SawboneAlleyInTooDeep
sawboneAlleyInTooDeep = locationWith SawboneAlleyInTooDeep Cards.sawboneAlleyInTooDeep 4 (PerPlayer 2) connectsToAdjacent

instance HasAbilities SawboneAlleyInTooDeep where
  getAbilities (SawboneAlleyInTooDeep attrs) =
    extendRevealed
      attrs
      [ skillTestAbility $ restricted attrs 1 Here parleyAction_
      , groupLimit PerGame
          $ restricted attrs 2 (Here <> HasCalculation (InvestigatorKeyCountCalculation Anyone) (atLeast 7))
          $ FastAbility' Free [#parley]
      ]

instance RunMessage SawboneAlleyInTooDeep where
  runMessage msg l@(SawboneAlleyInTooDeep attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #intellect (GameValueCalculation $ PerPlayer 1)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      removeBarrierBetweenConnected iid attrs.id
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      flashback iid Flashback7
      pure l
    _ -> SawboneAlleyInTooDeep <$> liftRunMessage msg attrs
