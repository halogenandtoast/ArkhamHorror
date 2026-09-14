module Arkham.Enemy.Cards.FortuneAndFolly.DimensionalDuplicatorB (dimensionalDuplicatorB) where

import Arkham.Ability
import Arkham.Card
import Arkham.Enemy.CardDefs.FortuneAndFolly qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Placement
import Arkham.Projection

newtype DimensionalDuplicatorB = DimensionalDuplicatorB EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dimensionalDuplicatorB :: EnemyCard DimensionalDuplicatorB
dimensionalDuplicatorB = enemy DimensionalDuplicatorB Cards.dimensionalDuplicatorB

instance HasAbilities DimensionalDuplicatorB where
  getAbilities (DimensionalDuplicatorB a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ EnemyDealtDamage #after AnyDamageEffect (be a) (SourceUsedBy You)

instance RunMessage DimensionalDuplicatorB where
  runMessage msg e@(DimensionalDuplicatorB attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      Deck deck <- field InvestigatorDeck iid
      for_ (nonEmpty deck) \(card :| _) -> do
        let extradimensionalEnemy = PlayerCard $ card {pcCardCode = "xextra"}
        push $ RemovePlayerCardFromGame False $ PlayerCard card
        createEnemy_ extradimensionalEnemy (InThreatArea iid)
      pure e
    _ -> DimensionalDuplicatorB <$> liftRunMessage msg attrs
