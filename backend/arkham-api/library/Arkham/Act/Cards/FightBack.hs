module Arkham.Act.Cards.FightBack (fightBack) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted hiding (EnemyAttacks)
import Arkham.Act.Sequence
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.SkillTest (withSkillTest)
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
      [ limitedAbility (MaxPer Cards.fightBack PerTest 1)
          $ restricted a 1 attackAtYourLocation
          $ FastAbility (ClueCost $ Static 1)
      , restricted a 2 (InVictoryDisplay (CardWithTrait Cthulhu) (atLeast 3))
          $ Objective
          $ forced
          $ RoundEnds #when
      ]

instance RunMessage FightBack where
  runMessage msg a@(FightBack attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> do
        skillTestModifier sid (attrs.ability 1) iid (DamageDealt 1)
      pure a
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      returnCthulhuFacetsToBoard
      increaseCthulhuRage 1
      rage <- (+ 1) <$> getCthulhuRage

      if rage <= 4
        then push $ RevertAct attrs.id
        else push R1
      pure a
    RevertAct aid | aid == attrs.id && onSide B attrs -> do
      pure $ FightBack $ attrs & sequenceL .~ Sequence 1 A
    _ -> FightBack <$> liftRunMessage msg attrs
