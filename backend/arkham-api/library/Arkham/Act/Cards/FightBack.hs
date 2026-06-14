module Arkham.Act.Cards.FightBack (fightBack) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted hiding (EnemyAttacks)
import Arkham.Act.Sequence
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.Window (getAttackDetails)
import Arkham.Matcher
import Arkham.Scenarios.TheDoomOfArkhamPartII.Helpers
import Arkham.Trait (Trait (Cthulhu))

newtype FightBack = FightBack ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fightBack :: ActCard FightBack
fightBack = act (1, A) FightBack Cards.fightBack Nothing

instance HasAbilities FightBack where
  getAbilities (FightBack a) =
    extend
      a
      [ -- "[fast] During an attack at your location, spend 1 clue: That attack
        -- deals +1 damage. (Max once per attack.)"
        limitedAbility (MaxPer Cards.fightBack PerAttack 1)
          $ mkAbility a 1
          $ triggered (EnemyAttacks #when You AnyEnemyAttack AnyEnemy) (ClueCost (Static 1))
      , -- "Objective - When the round ends, if there are 3 [[Cthulhu]] enemies
        -- in the victory display, advance."
        restricted a 2 (InVictoryDisplay (CardWithTrait Cthulhu) (atLeast 3))
          $ Objective
          $ forced
          $ RoundEnds #when
      ]

instance RunMessage FightBack where
  runMessage msg a@(FightBack attrs) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 (getAttackDetails -> attack) _ -> do
      -- Boost the in-progress attack by 1 damage for the duration of the
      -- attack window.
      enemyAttackModifier (attrs.ability 1) attack.enemy (DamageDealt 1)
      pure a
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      -- "Return each Cthulhu enemy in the victory display to the Cthulhu Board
      -- non-Enraged."
      -- TODO: the Cthulhu Board (the shared zone holding the four facets, with
      -- Enraged / non-Enraged orientation) has no engine support yet, so the
      -- facets cannot actually be moved back from the victory display onto it.

      -- "Increase Cthulhu's Rage by 1." (queued; the meta update happens when
      -- the scenario processes the message, so reason about the new value
      -- ourselves rather than re-reading the rage here.)
      rage <- getCthulhuRage
      scenarioSpecific "increaseCthulhuRage" ()

      -- If the new Rage (old + 1) is 4 or less, flip back to act 1a; otherwise
      -- (Rage 5+) the city is lost (R1).
      if rage + 1 <= 4
        then push $ RevertAct attrs.id
        else push R1
      pure a
    RevertAct aid | aid == attrs.id && onSide B attrs -> do
      pure $ FightBack $ attrs & sequenceL .~ Sequence 1 A
    _ -> FightBack <$> liftRunMessage msg attrs
