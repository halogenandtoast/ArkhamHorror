module Arkham.Skill.Cards.DreamsOfTheClay1 (dreamsOfTheClay1) where

import Arkham.Ability
import {-# SOURCE #-} Arkham.GameEnv (getSkillTest)
import Arkham.Helpers.Window (cardDrawn)
import Arkham.Matcher
import Arkham.Message.Lifted.Placement
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted
import Arkham.SkillTestResult

newtype DreamsOfTheClay1 = DreamsOfTheClay1 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dreamsOfTheClay1 :: SkillCard DreamsOfTheClay1
dreamsOfTheClay1 = skill DreamsOfTheClay1 Cards.dreamsOfTheClay1

instance HasAbilities DreamsOfTheClay1 where
  getAbilities (DreamsOfTheClay1 x) =
    [ controlled_ x 1
        $ triggered
          (DrawCard #when You (basic $ NonPeril <> IsEncounterCard <> #treachery) EncounterDeck)
          (removeCost x)
    ]

instance RunMessage DreamsOfTheClay1 where
  runMessage msg s@(DreamsOfTheClay1 attrs) = runQueueT $ case msg of
    CheckSkillTestResultOptions skillTestId exclusions -> do
      mst <- getSkillTest
      for_ mst \st -> do
        when (st.id == skillTestId && isTarget attrs st.target) do
          case st.result of
            SucceededBy {} -> do
              provideSkillTestResultOption attrs exclusions "Dreams of the Clay (1)" do
                place attrs (InPlayArea attrs.owner)
            _ -> pure ()
      pure s
    UseCardAbility _ (isSource attrs -> True) 1 (cardDrawn -> card) _ -> do
      cancelRevelation attrs card
      pure s
    _ -> DreamsOfTheClay1 <$> liftRunMessage msg attrs
