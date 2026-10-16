module Arkham.Homebrew.TheInvisibleMan.Enemies.SentientSolution (sentientSolution) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.Source (sourceTraits)
import Arkham.Helpers.Window.Damage (getDamageSource)
import Arkham.Homebrew.TheInvisibleMan.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.TheInvisibleMan.Helpers
import Arkham.Matcher
import Arkham.Trait (Trait (Firearm, Melee, Ranged, Spell))

newtype SentientSolution = SentientSolution EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sentientSolution :: EnemyCard SentientSolution
sentientSolution = enemy SentientSolution Cards.sentientSolution

-- | The four traits the ability reads; "that _Trait_" is whichever of them the asset has.
solventTraits :: [Trait]
solventTraits = [Melee, Ranged, Firearm, Spell]

instance HasAbilities SentientSolution where
  getAbilities (SentientSolution a) =
    extend1 a
      $ scenarioI18n
      $ withI18nTooltip "sentientSolution.adapt"
      $ mkAbility a 1
      $ forced
      $ EnemyDealtDamage #after AnyDamageEffect (be a)
      $ SourceIsAsset
      $ mapOneOf AssetWithTrait solventTraits

instance RunMessage SentientSolution where
  runMessage msg e@(SentientSolution attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (getDamageSource -> source) _ -> do
      traits <- sourceTraits source
      let shared = filter (`elem` traits) solventTraits
      when (notNull shared) do
        roundModifier (attrs.ability 1) attrs
          $ CannotBeDamagedByPlayerSources
          $ SourceIsAsset
          $ mapOneOf AssetWithTrait shared
      pure e
    _ -> SentientSolution <$> liftRunMessage msg attrs
