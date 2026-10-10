module Arkham.Homebrew.AgesUnwound.Enemies.Irregulars (irregulars) where

import Arkham.Ability
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Cards
import Arkham.Matcher

newtype Irregulars = Irregulars EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

irregulars :: EnemyCard Irregulars
irregulars = enemy Irregulars Cards.irregulars

{- | "Forced - After the Irregulars enter play: They get +2 fight, +2 evade and
+1 horror until the end of the round." / "Forced - After the Irregulars attack
you: Shuffle them into the top 2[per_investigator] cards of the encounter deck."
-}
instance HasAbilities Irregulars where
  getAbilities (Irregulars a) =
    extend
      a
      [ mkAbility a 1 $ forced $ EnemyEntersPlay #after (be a)
      , mkAbility a 2 $ forced $ EnemyAttacks #after You AnyEnemyAttack (be a)
      ]

instance RunMessage Irregulars where
  runMessage msg e@(Irregulars attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      roundModifiers (attrs.ability 1) attrs [EnemyFight 2, EnemyEvade 2, HorrorDealt 1]
      pure e
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      n <- perPlayer 2
      push $ RemoveFromPlay (toSource attrs)
      shuffleCardsIntoTopOfDeck Deck.EncounterDeck n [toCard attrs]
      pure e
    _ -> Irregulars <$> liftRunMessage msg attrs
