module Arkham.Homebrew.AgesUnwound.Treacheries.OutOfPhase (outOfPhase) where

import Arkham.Ability
import Arkham.Card
import Arkham.Helpers.Enemy (createEngagedWith)
import Arkham.Helpers.Modifiers (ModifierType (..), modified_, modifySelect)
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Placement
import Arkham.Treachery.Import.Lifted

newtype OutOfPhase = OutOfPhase TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

outOfPhase :: TreacheryCard OutOfPhase
outOfPhase = treachery OutOfPhase Cards.outOfPhase

{- | "You cannot interact with cards in other investigators' threat areas, and
vice versa."

"Interact" is not an Arkham keyword. What the rules do grant, and what this
therefore takes away, is the permission to use a triggered ability on "all
encounter cards in the threat area of any investigator at that location"
(@mcp/references/rules/glossary/triggered_abilities.md@), plus the obvious
reading that you may no longer fight or evade an enemy sitting in someone
else's threat area.

TODO(ages-unwound): the ability half only covers enemies. 'AbilityMatcher' has
no treachery constructor, so an ability on a /treachery/ in another
investigator's threat area is still offered.
-}
instance HasModifiersFor OutOfPhase where
  getModifiersFor (OutOfPhase a) = case a.placement of
    InThreatArea iid -> do
      let theirs = EnemyIsEngagedWith (not_ $ InvestigatorWithId iid)
      let ours = enemyInThreatAreaOf iid
      modified_
        a
        iid
        [CannotFight theirs, CannotEvade theirs, CannotTriggerAbilityMatching (AbilityOnEnemy theirs)]
      modifySelect
        a
        (not_ $ InvestigatorWithId iid)
        [CannotFight ours, CannotEvade ours, CannotTriggerAbilityMatching (AbilityOnEnemy ours)]
    _ -> pure mempty

-- | "Forced - At the end of the round: Discard Out of Phase."
instance HasAbilities OutOfPhase where
  getAbilities (OutOfPhase a) = [mkAbility a 1 $ forced $ RoundEnds #when]

{- | "Revelation - Test [willpower] (4). If you fail, put Out of Phase into play
in your threat area. If you fail by 3 or more, search the encounter deck and
discard pile for an enemy and spawn it engaged with you."
-}
instance RunMessage OutOfPhase where
  runMessage msg t@(OutOfPhase attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 4)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      placeInThreatArea attrs iid
      when (n >= 3) $ findEncounterCardIn iid attrs (card_ #enemy) [#deck, #discard]
      pure t
    FoundEncounterCard iid (isTarget attrs -> True) (toCard -> card) -> do
      createEnemyWith_ card Unplaced $ createEngagedWith iid
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      toDiscard (attrs.ability 1) attrs
      pure t
    _ -> OutOfPhase <$> liftRunMessage msg attrs
