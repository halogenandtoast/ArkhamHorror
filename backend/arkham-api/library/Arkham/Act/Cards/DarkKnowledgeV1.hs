module Arkham.Act.Cards.DarkKnowledgeV1 (darkKnowledgeV1) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Creation
import Arkham.Matcher
import Arkham.Scenarios.InTheClutchesOfChaos.Helpers

newtype DarkKnowledgeV1 = DarkKnowledgeV1 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darkKnowledgeV1 :: ActCard DarkKnowledgeV1
darkKnowledgeV1 = act (1, A) DarkKnowledgeV1 Cards.darkKnowledgeV1 Nothing

-- The remove 3 breaches cost is so specific we just handle it in the ability,
-- if this becomes a timing issue later on we might want something like
-- `AbilityCost Bool` where Bool specifies whether or not it can be paid (has
-- to be based on state). Afterwards have some sort of `PayAbilityCost` message
-- that handles the Payment, but we won't be able to track the amount paid

instance HasAbilities DarkKnowledgeV1 where
  getAbilities (DarkKnowledgeV1 x) =
    extend
      x
      [ fastAbility x 1 Free $ if maybe False (>= 3) (actBreaches x) then NoRestriction else Never
      , mkAbility x 2 $ Objective $ ForcedAbilityWithCost AnyWindow (GroupClueCost (PerPlayer 3) Anywhere)
      ]

instance RunMessage DarkKnowledgeV1 where
  runMessage msg a@(DarkKnowledgeV1 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      removeBreaches attrs 3
      location <- sampleLocation
      placeClues (attrs.ability 1) location 1
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithClues attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      -- NOTE: moving the breaches is a bit of a hack as we use the "known" act id
      -- need to make sure this still works for the return to
      anetteMason <- fetchCard Enemies.anetteMasonReincarnatedEvil
      let breaches = fromMaybe 0 (actBreaches attrs)
      createEnemy_ anetteMason SpawnViaSpawnInstruction
      advanceActDeck attrs
      when (breaches > 0) do
        placeBreaches (ActTarget $ ActId $ toCardCode Acts.beyondTheGrave) breaches
      pure a
    _ -> DarkKnowledgeV1 <$> liftRunMessage msg attrs
