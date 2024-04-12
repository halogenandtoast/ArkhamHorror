module Arkham.Treachery.Cards.FalseAwakening (falseAwakening, FalseAwakening (..)) where

import Arkham.Ability
import Arkham.Classes
import Arkham.Helpers.SkillTest qualified as Msg
import Arkham.Message.Lifted
import Arkham.Prelude
import Arkham.SkillType
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner hiding (chooseOne)

-- NOTE: False Awakening's constant ability of starting next to the agenda deck
-- is hard coded (Investigator/Runner and Scenario/Runner). If we have another
-- card that does this we'll want to encode this somehome

newtype FalseAwakening = FalseAwakening TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

falseAwakening :: TreacheryCard FalseAwakening
falseAwakening = treachery FalseAwakening Cards.falseAwakening

instance HasAbilities FalseAwakening where
  getAbilities (FalseAwakening a) = [mkAbility a 1 actionAbility]

instance RunMessage FalseAwakening where
  runMessage msg t@(FalseAwakening attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) ->
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      n <- perPlayer 1
      chooseOne
        iid
        [SkillLabel s [Msg.beginSkillTest iid (attrs.ability 1) iid s (Fixed $ 2 + n)] | s <- allSkills]
      pure t
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      push $ RemoveFromGame (toTarget attrs)
      pure t
    _ -> FalseAwakening <$> lift (runMessage msg attrs)
