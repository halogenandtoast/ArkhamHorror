module Arkham.Treachery.Cards.ChildrenOfValusia (childrenOfValusia) where

import Arkham.Ability
import Arkham.Classes
import Arkham.Helpers.Modifiers (ModifierType (EnemyEvade, EnemyFight), modifySelect)
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Trait (Trait (Serpent))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype ChildrenOfValusia = ChildrenOfValusia TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

childrenOfValusia :: TreacheryCard ChildrenOfValusia
childrenOfValusia = treachery ChildrenOfValusia Cards.childrenOfValusia

instance HasModifiersFor ChildrenOfValusia where
  getModifiersFor (ChildrenOfValusia a) =
    modifySelect a (EnemyWithTrait Serpent) [EnemyFight 1, EnemyEvade 1]

instance HasAbilities ChildrenOfValusia where
  getAbilities (ChildrenOfValusia a) =
    [ limited (MaxPer Cards.childrenOfValusia PerRound 1)
        $ mkAbility a 1
        $ forced
        $ RoundEnds #when
    ]

instance RunMessage ChildrenOfValusia where
  runMessage msg t@(ChildrenOfValusia attrs) = case msg of
    Revelation _iid source | isSource attrs source -> do
      push $ PlaceTreachery (toId attrs) NextToAgenda
      pure t
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      push $ toDiscard (toAbilitySource attrs 1) attrs
      pure t
    _ -> ChildrenOfValusia <$> runMessage msg attrs
