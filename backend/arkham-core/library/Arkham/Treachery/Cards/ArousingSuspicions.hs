module Arkham.Treachery.Cards.ArousingSuspicions (
  ArousingSuspicions (..),
  arousingSuspicions,
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype ArousingSuspicions = ArousingSuspicions TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

arousingSuspicions :: TreacheryCard ArousingSuspicions
arousingSuspicions = treachery ArousingSuspicions Cards.arousingSuspicions

instance RunMessage ArousingSuspicions where
  runMessage msg t@(ArousingSuspicions attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      criminals <-
        selectList
          $ enemyAtLocationWith iid
          <> EnemyWithTrait Criminal
          <> EnemyWithoutModifier CannotPlaceDoomOnThis
      if null criminals
        then push (SpendResources iid 2)
        else pushAll [PlaceDoom (toSource attrs) (toTarget eid) 1 | eid <- criminals]
      pure t
    _ -> ArousingSuspicions <$> runMessage msg attrs
