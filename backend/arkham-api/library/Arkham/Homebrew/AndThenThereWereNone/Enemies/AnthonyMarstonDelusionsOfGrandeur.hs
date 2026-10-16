{- | The Playboy, unmasked. He is immune while the sigils are loaded, and every
hit that does land loads them further.
-}
module Arkham.Homebrew.AndThenThereWereNone.Enemies.AnthonyMarstonDelusionsOfGrandeur (
  anthonyMarstonDelusionsOfGrandeur,
) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.AndThenThereWereNone.Helpers
import Arkham.Matcher

newtype AnthonyMarstonDelusionsOfGrandeur = AnthonyMarstonDelusionsOfGrandeur EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

anthonyMarstonDelusionsOfGrandeur :: EnemyCard AnthonyMarstonDelusionsOfGrandeur
anthonyMarstonDelusionsOfGrandeur =
  enemy AnthonyMarstonDelusionsOfGrandeur Cards.anthonyMarstonDelusionsOfGrandeur

instance HasModifiersFor AnthonyMarstonDelusionsOfGrandeur where
  -- "While there are 2{perPlayer} or more total offerings among all copies of
  -- Antediluvian Sigils, Anthony Marston cannot be damaged."
  getModifiersFor (AnthonyMarstonDelusionsOfGrandeur a) = do
    total <- totalOfferings
    threshold <- perPlayer 2
    modifySelfWhen a (total >= threshold) [CannotBeDamaged]

instance HasAbilities AnthonyMarstonDelusionsOfGrandeur where
  getAbilities (AnthonyMarstonDelusionsOfGrandeur a) =
    extend1 a
      $ scenarioI18n
      $ withI18nTooltip "anthonyMarstonDelusionsOfGrandeur.offering"
      $ mkAbility a 1
      $ forced
      $ EnemyDealtDamage #after AnyDamageEffect (be a) AnySource

instance RunMessage AnthonyMarstonDelusionsOfGrandeur where
  runMessage msg e@(AnthonyMarstonDelusionsOfGrandeur attrs) = runQueueT $ case msg of
    -- "Add 1 offering to a copy of Antediluvian Sigils (without investigators
    -- at its location, if possible)."
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      addOfferingToSigil (attrs.ability 1) iid
      pure e
    _ -> AnthonyMarstonDelusionsOfGrandeur <$> liftRunMessage msg attrs
