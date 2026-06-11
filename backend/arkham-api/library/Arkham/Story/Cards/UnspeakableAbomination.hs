module Arkham.Story.Cards.UnspeakableAbomination (unspeakableAbomination) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype UnspeakableAbomination = UnspeakableAbomination StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unspeakableAbomination :: StoryCard UnspeakableAbomination
unspeakableAbomination = story UnspeakableAbomination Cards.unspeakableAbomination & persistStory

instance HasAbilities UnspeakableAbomination where
  getAbilities (UnspeakableAbomination a) =
    if a ^. flippedL
      then
        [ restricted
            a
            1
            (youExist $ InvestigatorAt (LocationWithEnemy $ enemyIs Enemies.tyrthrha))
            $ actionAbilityWithCost (ClueCost (Static 1))
        , mkAbility a 2 $ forced $ EnemyDefeated #when Anyone ByAny (enemyIs Enemies.tyrthrha)
        ]
      else []

instance RunMessage UnspeakableAbomination where
  runMessage msg s@(UnspeakableAbomination attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #intellect (Fixed 3)
      pure s
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n -> do
      selectForMaybeM (enemyIs Enemies.tyrthrha) \tyrthrha -> do
        let damage = if n >= 3 then 3 else 2
        nonAttackEnemyDamage (Just iid) (attrs.ability 1) damage tyrthrha
      pure s
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      addToVictory iid attrs
      pure s
    Flip _ _ (isTarget attrs -> True) -> do
      tindalos <- selectOne $ locationIs Locations.tindalos
      for_ tindalos \tindalos' -> do
        tyrthrha <- getSetAsideCardsMatching (cardIs Enemies.tyrthrha)
        for_ tyrthrha \card -> createEnemyAt_ card tindalos'
      pure $ UnspeakableAbomination $ attrs & flippedL .~ True
    _ -> UnspeakableAbomination <$> liftRunMessage msg attrs
