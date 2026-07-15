module Arkham.Homebrew.CircusExMortis.Enemies.DisguisedMonstrosity (disguisedMonstrosity) where

import Arkham.Ability
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher

newtype DisguisedMonstrosity = DisguisedMonstrosity EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

disguisedMonstrosity :: EnemyCard DisguisedMonstrosity
disguisedMonstrosity = enemy DisguisedMonstrosity Cards.disguisedMonstrosity

instance HasModifiersFor DisguisedMonstrosity where
  getModifiersFor (DisguisedMonstrosity a) = do
    modifySelf a [AddKeyword Keyword.Hunter, AddKeyword Keyword.Massive]
    exposed <- selectAny $ locationWithEnemy a <> RevealedLocation <> LocationWithoutClues
    when exposed $ modifySelf a [EnemyFight (-1), DamageTakenFrom AttackDamageEffect 1]

instance HasAbilities DisguisedMonstrosity where
  getAbilities (DisguisedMonstrosity a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ EnemyMoves #after (RevealedLocation <> LocationWithoutClues) (be a)

instance RunMessage DisguisedMonstrosity where
  runMessage msg e@(DisguisedMonstrosity attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      exhaustThis attrs
      pure e
    _ -> DisguisedMonstrosity <$> liftRunMessage msg attrs
