module Arkham.Homebrew.AgainstTheWendigo.Enemies.Bear (bear) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModifySelf)
import Arkham.Helpers.Query (getLead)
import Arkham.Helpers.SkillTest (getSkillTestSource)
import Arkham.Helpers.Source (sourceMatches)
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.Helpers (isSymbolFace)
import Arkham.Homebrew.AgainstTheWendigo.Traits (pattern Wild)
import Arkham.Matcher
import Arkham.Message.Lifted.Move (enemyMoveToMatch)
import Arkham.Trait (Trait (Firearm, Ranged, Spell))

newtype Bear = Bear EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bear :: EnemyCard Bear
bear = enemy Bear Cards.bear & setSpawnAt (NearestLocationToYou $ LocationWithTrait Wild)

instance HasModifiersFor Bear where
  getModifiersFor (Bear a) = maybeModifySelf a do
    source <- MaybeT getSkillTestSource
    ensure $ sourceMatches source $ SourceMatchesAny [SourceWithTrait t | t <- [Spell, Firearm, Ranged]]
    pure [EnemyFight (-1)]

instance HasAbilities Bear where
  getAbilities (Bear a) = extend1 a $ mkAbility a 1 $ forced $ PhaseEnds #when #enemy

instance RunMessage Bear where
  runMessage msg e@(Bear attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      lead <- getLead
      requestChaosTokens lead (attrs.ability 1) 1
      pure e
    RequestedChaosTokens (isAbilitySource attrs 1 -> True) miid tokens -> do
      for_ miid continue_
      if any isSymbolFace tokens
        then withLocationOf attrs \lid ->
          enemyMoveToMatch attrs attrs (NearestLocationToLocation lid $ LocationWithInvestigator Anyone)
        else toDiscard (attrs.ability 1) attrs
      pure e
    _ -> Bear <$> liftRunMessage msg attrs
