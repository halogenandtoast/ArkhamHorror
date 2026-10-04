module Arkham.Homebrew.AgainstTheWendigo.Enemies.Bear (bear) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
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
  {- | "If you fight the Bear with a Spell, Firearm or Ranged asset, Bear loses
  -1 [combat]" -- read as -1 fight for that attack, so it only applies while
  such a source is the one being tested with.
  -}
  getModifiersFor (Bear a) = do
    armed <-
      getSkillTestSource >>= \case
        Nothing -> pure False
        Just source ->
          sourceMatches source $ SourceMatchesAny [SourceWithTrait t | t <- [Spell, Firearm, Ranged]]
    modifySelfWhen a armed [EnemyFight (-1)]

instance HasAbilities Bear where
  getAbilities (Bear a) = [mkAbility a 1 $ forced $ PhaseEnds #when #enemy]

instance RunMessage Bear where
  runMessage msg e@(Bear attrs) = runQueueT $ case msg of
    -- "At the end of each enemy phase, reveal a chaos token: on a symbol, the
    -- Bear moves toward the nearest investigator. Otherwise discard this card."
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      lead <- getLead
      requestChaosTokens lead (attrs.ability 1)  1
      pure e
    RequestedChaosTokens (isAbilitySource attrs 1 -> True) _ tokens -> do
      if any isSymbolFace tokens
        then enemyMoveToMatch attrs attrs (NearestLocationToYou Anywhere)
        else push $ ShuffleBackIntoEncounterDeck (toSource attrs) (toTarget attrs)
      pure e
    _ -> Bear <$> liftRunMessage msg attrs
