module Arkham.Homebrew.AgainstTheWendigo.Enemies.SylviaDavidson (sylviaDavidson) where

import Arkham.Ability
import Arkham.Attack (enemyAttack)
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Enemies qualified as Cards
import Arkham.Matcher

newtype SylviaDavidson = SylviaDavidson EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sylviaDavidson :: EnemyCard SylviaDavidson
sylviaDavidson = enemy SylviaDavidson Cards.sylviaDavidson

instance HasModifiersFor SylviaDavidson where
  getModifiersFor (SylviaDavidson a) = do
    n <- getPlayerCount
    {- | "Sylvia Davidson gains +1 [per_investigator] health." and "You cannot
    fight or evade Sylvia Davidson, unless you perform a Fight or Evade action
    on a Spell card, or the following actions" -- which are her own abilities, so
    the exception is any Spell source or her own. -}
    modifySelf
      a
      [ HealthModifier n
      , CannotBeAttackedByPlayerSourcesExcept $ SourceMatchesAny [#spell, SourceIsEnemy (be a)]
      , CannotBeEvadedByPlayerSourcesExcept $ SourceMatchesAny [#spell, SourceIsEnemy (be a)]
      ]

instance HasAbilities SylviaDavidson where
  getAbilities (SylviaDavidson a) =
    [ fightAbility a 1 (ActionCost 1) OnSameLocation
    , evadeAbility a 2 (ActionCost 1) OnSameLocation
    , -- "At the enemy phase: Sylvia attacks the investigator with the lowest
      -- remaining sanity in her location (without engaging her or him)."
      restricted a 3 (exists $ InvestigatorAt $ locationWithEnemy a) $ forced $ PhaseBegins #when #enemy
    ]

instance RunMessage SylviaDavidson where
  runMessage msg e@(SylviaDavidson attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseFightEnemyMatch sid iid (attrs.ability 1) (be attrs)
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      chooseEvadeEnemyMatch sid iid (attrs.ability 2) (be attrs)
      pure e
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      targets <- select $ InvestigatorAt (locationWithEnemy attrs) <> LowestRemainingSanity
      for_ (take 1 targets) \iid -> push $ InitiateEnemyAttack $ enemyAttack (toId attrs) attrs iid
      pure e
    _ -> SylviaDavidson <$> liftRunMessage msg attrs
