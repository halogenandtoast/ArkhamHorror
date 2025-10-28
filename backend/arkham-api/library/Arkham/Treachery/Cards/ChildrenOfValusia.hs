module Arkham.Treachery.Cards.ChildrenOfValusia (childrenOfValusia) where

import Arkham.Ability
import Arkham.Placement
import Arkham.Helpers.Modifiers (ModifierType (EnemyEvade, EnemyFight), modifySelect)
import Arkham.Matcher
import Arkham.Trait (Trait (Serpent))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

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
    [limited (MaxPer Cards.childrenOfValusia PerRound 1) $ mkAbility a 1 $ forced $ RoundEnds #when]

instance RunMessage ChildrenOfValusia where
  runMessage msg t@(ChildrenOfValusia attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      placeTreachery (toId attrs) NextToAgenda
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> ChildrenOfValusia <$> liftRunMessage msg attrs
