module Arkham.Homebrew.AgainstTheWendigo.Enemies.TerrifiedPoliceman (terrifiedPoliceman) where

import Arkham.Ability
import Arkham.Attack (enemyAttack)
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.Traits (pattern Wild)
import Arkham.Matcher

newtype TerrifiedPoliceman = TerrifiedPoliceman EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

terrifiedPoliceman :: EnemyCard TerrifiedPoliceman
terrifiedPoliceman =
  enemy TerrifiedPoliceman Cards.terrifiedPoliceman
    & setSpawnAt (FarthestLocationFromAll $ EmptyLocation <> LocationWithTrait Wild)

instance HasAbilities TerrifiedPoliceman where
  getAbilities (TerrifiedPoliceman a) =
    [ {- | "At the end of each investigator turn, if Terrified Policeman is not
      engaged: Terrified Policeman attacks the investigator in the same location,
      or in a location directly to the North, South, East or West." The valley's
      compass connections are the grid's, so the printed reach is the connected
      locations. -}
      restricted a 1 (notExists $ be a <> EnemyIsEngagedWith Anyone)
        $ forced
        $ TurnEnds #when Anyone
    , skillTestAbility $ restricted a 2 OnSameLocation parleyAction_
    ]

instance RunMessage TerrifiedPoliceman where
  runMessage msg e@(TerrifiedPoliceman attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      targets <-
        select
          $ InvestigatorAt
          $ oneOf [locationWithEnemy attrs, connectedFrom (locationWithEnemy attrs)]
      for_ (take 1 targets) \iid -> push $ InitiateEnemyAttack $ enemyAttack (toId attrs) attrs iid
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 2) attrs #intellect (Fixed 4)
      pure e
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      addToVictory iid attrs
      pure e
    FailedThisSkillTest _ (isAbilitySource attrs 2 -> True) -> do
      push $ ShuffleBackIntoEncounterDeck (toSource attrs) (toTarget attrs)
      pure e
    _ -> TerrifiedPoliceman <$> liftRunMessage msg attrs
