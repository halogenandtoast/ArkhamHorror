module Arkham.Skill.Cards.DreamsOfTheClay1 (dreamsOfTheClay1) where

import Arkham.Ability
import Arkham.Helpers.Window (cardDrawn)
import Arkham.Matcher
import Arkham.Message.Lifted.Placement
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype DreamsOfTheClay1 = DreamsOfTheClay1 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dreamsOfTheClay1 :: SkillCard DreamsOfTheClay1
dreamsOfTheClay1 = skill DreamsOfTheClay1 Cards.dreamsOfTheClay1

instance HasAbilities DreamsOfTheClay1 where
  getAbilities (DreamsOfTheClay1 x) =
    [ restricted x 1 ControlsThis
        $ triggered (DrawCard #when You (basic $ NonPeril <> IsEncounterCard) EncounterDeck) (removeCost x)
    ]

instance RunMessage DreamsOfTheClay1 where
  runMessage msg s@(DreamsOfTheClay1 attrs) = runQueueT $ case msg of
    PassedSkillTest _ _ _ (isTarget attrs -> True) _ _ -> do
      place attrs (InPlayArea attrs.owner)
      pure s
    UseCardAbility _ (isSource attrs -> True) 1 (cardDrawn -> card) _ -> do
      cancelRevelation attrs card
      pure s
    _ -> DreamsOfTheClay1 <$> liftRunMessage msg attrs
