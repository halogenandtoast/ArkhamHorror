module Arkham.Enemy.Cards.DimensionalDuplicatorA (dimensionalDuplicatorA) where

import Arkham.Ability
import Arkham.Card
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Placement
import Arkham.Projection

newtype DimensionalDuplicatorA = DimensionalDuplicatorA EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dimensionalDuplicatorA :: EnemyCard DimensionalDuplicatorA
dimensionalDuplicatorA = enemy DimensionalDuplicatorA Cards.dimensionalDuplicatorA (3, Static 3, 3) (1, 1)

instance HasAbilities DimensionalDuplicatorA where
  getAbilities (DimensionalDuplicatorA a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ EnemyDealtDamage #after AnyDamageEffect (be a) (SourceOwnedBy You)

instance RunMessage DimensionalDuplicatorA where
  runMessage msg e@(DimensionalDuplicatorA attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      Deck deck <- field InvestigatorDeck iid
      for_ (nonEmpty deck) \(card :| _) -> do
        let extradimensionalEnemy = PlayerCard $ card {pcCardCode = "xextra"}
        push $ RemovePlayerCardFromGame False $ PlayerCard card
        createEnemy_ extradimensionalEnemy (InThreatArea iid)
      pure e
    _ -> DimensionalDuplicatorA <$> liftRunMessage msg attrs
