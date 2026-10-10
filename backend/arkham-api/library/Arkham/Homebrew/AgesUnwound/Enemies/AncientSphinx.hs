module Arkham.Homebrew.AgesUnwound.Enemies.AncientSphinx (ancientSphinx) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Enemy.Types (Field (EnemyClues))
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Story (readStory)
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.AgesUnwound.CardDefs.Stories qualified as Stories
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Treacheries
import Arkham.Homebrew.AgesUnwound.Missions.Helpers (completeTask)
import Arkham.Matcher
import Arkham.Projection

newtype AncientSphinx = AncientSphinx EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | Alert is on the def; /Keeper of Knowledge/ spawns it in Cairo.
ancientSphinx :: EnemyCard AncientSphinx
ancientSphinx = enemy AncientSphinx Cards.ancientSphinx

{- | "[action]: __Parley.__ You attempt the riddle of the sphinx. Test
[intellect] (4). If you succeed, place 1 of your clues on Ancient Sphinx, then
if there are at least 1[per_investigator] clues on Ancient Sphinx, flip this
card over and resolve its text. If you fail, Ancient Sphinx attacks you. /
__Forced__ - When Ancient Sphinx leaves play: Complete Keeper of Knowledge."

Ability 2's condition is the enemy's identity, which is why it sits inside the
window matcher: at the point the sphinx leaves play a criterion's @exists@ finds
nothing (see @project_defeated_enemy_condition_belongs_in_the_window@).
-}
instance HasAbilities AncientSphinx where
  getAbilities (AncientSphinx a) =
    extend
      a
      [ skillTestAbility $ restricted a 1 OnSameLocation parleyAction_
      , mkAbility a 2 $ forced $ EnemyLeavesPlay #when (be a)
      ]

instance RunMessage AncientSphinx where
  runMessage msg e@(AncientSphinx attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #intellect (Fixed 4)
      pure e
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      moveTokens (attrs.ability 1) iid attrs #clue 1
      -- The riddle is checked after the clue has landed, hence the second step.
      doStep 1 msg
      pure e
    DoStep 1 (PassedThisSkillTest iid (isAbilitySource attrs 1 -> True)) -> do
      clues <- field EnemyClues attrs.id
      required <- perPlayer 1
      when (clues >= required) $ flipOverBy iid (attrs.ability 1) attrs
      pure e
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid attrs Stories.answers
      pure e
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      initiateEnemyAttack attrs (attrs.ability 1) iid
      pure e
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      completeTask Treacheries.keeperOfKnowledge
      pure e
    _ -> AncientSphinx <$> liftRunMessage msg attrs
