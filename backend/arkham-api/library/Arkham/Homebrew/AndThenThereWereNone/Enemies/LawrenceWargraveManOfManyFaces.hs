{- | The Judge, unmasked. He cannot be killed while a single offering remains,
and every card the party draws puts another one down.
-}
module Arkham.Homebrew.AndThenThereWereNone.Enemies.LawrenceWargraveManOfManyFaces (
  lawrenceWargraveManOfManyFaces,
) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.AndThenThereWereNone.Helpers
import Arkham.Matcher

newtype LawrenceWargraveManOfManyFaces = LawrenceWargraveManOfManyFaces EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lawrenceWargraveManOfManyFaces :: EnemyCard LawrenceWargraveManOfManyFaces
lawrenceWargraveManOfManyFaces =
  enemy LawrenceWargraveManOfManyFaces Cards.lawrenceWargraveManOfManyFaces

instance HasModifiersFor LawrenceWargraveManOfManyFaces where
  -- "While there are 1 or more offerings among copies of Antediluvian Sigils,
  -- Lawrence Wargrave cannot be defeated."
  getModifiersFor (LawrenceWargraveManOfManyFaces a) = do
    total <- totalOfferings
    modifySelfWhen a (total >= 1) [CannotBeDefeated]

instance HasAbilities LawrenceWargraveManOfManyFaces where
  -- "After an investigator draws 1 or more cards during their turn": the
  -- drawing investigator must be the one taking the turn, which is what
  -- TurnInvestigator says and what a draw handed out on someone else's turn is
  -- not.
  getAbilities (LawrenceWargraveManOfManyFaces a) =
    extend1 a
      $ scenarioI18n
      $ withI18nTooltip "lawrenceWargraveManOfManyFaces.offering"
      $ mkAbility a 1
      $ forced
      $ DrawsCards #after TurnInvestigator AnyCards (atLeast 1)

instance RunMessage LawrenceWargraveManOfManyFaces where
  runMessage msg e@(LawrenceWargraveManOfManyFaces attrs) = runQueueT $ case msg of
    -- "Add 1 offering to a copy of Antediluvian Sigils (without investigators
    -- at its location, if possible)."
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      addOfferingToSigil (attrs.ability 1) iid
      pure e
    _ -> LawrenceWargraveManOfManyFaces <$> liftRunMessage msg attrs
