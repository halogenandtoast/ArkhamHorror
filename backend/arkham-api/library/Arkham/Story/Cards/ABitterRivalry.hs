module Arkham.Story.Cards.ABitterRivalry (aBitterRivalry) where

import Arkham.Ability
import Arkham.Actions (orActions)
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Query (getLead, getPlayerCount)
import Arkham.I18n
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Action (narrowTakenActions)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted
import Arkham.Token qualified as Token

newtype ABitterRivalry = ABitterRivalry StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aBitterRivalry :: StoryCard ABitterRivalry
aBitterRivalry = story ABitterRivalry Cards.aBitterRivalry & persistStory

edwin :: EnemyMatcher
edwin = enemyIs Enemies.edwinBennetBitterAdversary

tindalos :: LocationMatcher
tindalos = locationIs Locations.tindalos

instance HasAbilities ABitterRivalry where
  getAbilities (ABitterRivalry a) =
    guard a.flipped
      *> [ restricted a 1 (exists $ edwin <> notAt_ YourLocation) doubleActionAbility
         , groupLimit PerRound
             $ restricted a 2 (exists $ edwin <> at_ YourLocation)
             $ ActionAbility (orActions [#fight, #evade]) Nothing (ActionCost 1)
         , onlyOnce
             $ restricted a 3 (exists $ edwin <> at_ tindalos <> EnemyWithTokens (Static 3) Token.Target)
             $ forced AnyWindow
         ]

instance RunMessage ABitterRivalry where
  runMessage msg s@(ABitterRivalry attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withMatch edwin \edwin' -> do
        readyThis edwin'
        withLocationOf iid $ enemyMoveTo (attrs.ability 1) edwin'
      pure s
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withMatch edwin \edwin' -> do
        sid <- getRandom
        chooseOneM iid $ withI18n do
          labeled' "fight" do
            narrowTakenActions [#fight]
            fightEnemy sid iid (attrs.ability 2) edwin'
          labeled' "evade" do
            narrowTakenActions [#evade]
            chooseEvadeEnemyMatch sid iid (attrs.ability 2) (EnemyWithId edwin')
      pure s
    PassedThisSkillTest _iid (isAbilitySource attrs 2 -> True) -> do
      total <- getPlayerCount
      investigatorClues <- selectWithField InvestigatorClues UneliminatedInvestigator
      let totalClues = sum (map snd investigatorClues)
      when (totalClues >= total) do
        spendCluesAsAGroup (map fst investigatorClues) total
        selectForMaybeM edwin \edwin' ->
          placeTokens (attrs.ability 2) edwin' Token.Target 1
      doStep 1 msg
      pure s
    DoStep 1 (PassedThisSkillTest _ (isAbilitySource attrs 2 -> True)) -> do
      checkObjective attrs
      pure s
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      checkObjective attrs
      pure s
    Flip _ _ (isTarget attrs -> True) -> do
      lead <- getLead
      withLocationOf lead (createSetAsideEnemy_ Enemies.edwinBennetBitterAdversary)
      flippedOver attrs
      pure $ ABitterRivalry $ attrs & flippedL .~ True
    _ -> ABitterRivalry <$> liftRunMessage msg attrs

checkObjective :: ReverseQueue m => StoryAttrs -> m ()
checkObjective attrs = do
  mEdwin <- selectOne $ edwin <> at_ tindalos <> EnemyWithTokens (Static 3) Token.Target
  for_ mEdwin \edwin' -> do
    removeFromGame edwin'
    lead <- getLead
    addToVictory lead attrs
