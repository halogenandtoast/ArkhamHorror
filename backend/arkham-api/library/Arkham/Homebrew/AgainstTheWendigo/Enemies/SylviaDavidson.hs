module Arkham.Homebrew.AgainstTheWendigo.Enemies.SylviaDavidson (sylviaDavidson) where

import Arkham.Ability
import Arkham.Constants
import Arkham.Enemy.Import.Lifted
import Arkham.Fight (mkChooseFightPure, withSkillType)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Enemies qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype SylviaDavidson = SylviaDavidson EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sylviaDavidson :: EnemyCard SylviaDavidson
sylviaDavidson = enemy SylviaDavidson Cards.sylviaDavidson

instance HasModifiersFor SylviaDavidson where
  getModifiersFor (SylviaDavidson a) = do
    n <- getPlayerCount
    modifySelf
      a
      [ HealthModifier n
      , CannotBeAttackedByPlayerSourcesExcept exceptSpellOrHerOwn
      , CannotBeEvadedByPlayerSourcesExcept exceptSpellOrHerOwn
      ]
   where
    exceptSpellOrHerOwn = SourceMatchesAny [#spell, SourceIsEnemy (be a)]

instance HasAbilities SylviaDavidson where
  getAbilities (SylviaDavidson a) =
    map onlySpells (getAbilities a)
      <> [ restricted a 1 canFightCriteria $ ActionAbility #fight #willpower (ActionCost 1)
         , restricted a 2 EvadeCriteria $ ActionAbility #evade #willpower (ActionCost 1)
         , restricted a 3 (exists $ InvestigatorAt $ locationWithEnemy a) $ forced $ PhaseBegins #when #enemy
         ]
   where
    onlySpells ab
      | ab.index `elem` [AbilityAttack, AbilityEvade] =
          ab {abilityCriteria = ab.criteria <> OnlySources #spell}
      | otherwise = ab

instance RunMessage SylviaDavidson where
  runMessage msg e@(SylviaDavidson attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      push $ FightEnemy attrs.id $ withSkillType #willpower $ mkChooseFightPure sid iid (attrs.ability 1)
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      push $ EvadeEnemy sid iid attrs.id (attrs.ability 2) Nothing #willpower False
      pure e
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      investigators <- select $ InvestigatorAt (locationWithEnemy attrs) <> LowestRemainingSanity
      leadChooseOrRunOneM $ targets investigators (initiateEnemyAttack (toId attrs) attrs)
      pure e
    _ -> SylviaDavidson <$> liftRunMessage msg attrs
