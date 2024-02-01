module Arkham.Treachery.Cards.ChildrenOfValusia (
  childrenOfValusia,
  ChildrenOfValusia (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.Matcher
import Arkham.Timing qualified as Timing
import Arkham.Trait (Trait (Serpent))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype ChildrenOfValusia = ChildrenOfValusia TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

childrenOfValusia :: TreacheryCard ChildrenOfValusia
childrenOfValusia = treachery ChildrenOfValusia Cards.childrenOfValusia

instance HasModifiersFor ChildrenOfValusia where
  getModifiersFor (EnemyTarget eid) (ChildrenOfValusia a) = do
    isSerpent <- eid <=~> EnemyWithTrait Serpent
    pure $ toModifiers a $ do
      guard isSerpent
      [EnemyFight 1, EnemyEvade 1]
  getModifiersFor _ _ = pure []

instance HasAbilities ChildrenOfValusia where
  getAbilities (ChildrenOfValusia a) =
    [ limitedAbility (MaxPer Cards.childrenOfValusia PerRound 1)
        $ mkAbility a 1
        $ ForcedAbility
        $ RoundEnds Timing.When
    ]

instance RunMessage ChildrenOfValusia where
  runMessage msg t@(ChildrenOfValusia attrs) = case msg of
    Revelation _iid source | isSource attrs source -> do
      push $ PlaceTreachery (toId attrs) TreacheryNextToAgenda
      pure t
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      push $ toDiscard (toAbilitySource attrs 1) attrs
      pure t
    _ -> ChildrenOfValusia <$> runMessage msg attrs
