module Arkham.Enemy.Cards.NightOfTheZealot.GhoulsOfUmordhoth.AcolyteOfUmordhoth (acolyteOfUmordhoth) where

import Arkham.Enemy.CardDefs.NightOfTheZealot.GhoulsOfUmordhoth qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Matcher

newtype AcolyteOfUmordhoth = AcolyteOfUmordhoth EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

acolyteOfUmordhoth :: EnemyCard AcolyteOfUmordhoth
acolyteOfUmordhoth =
  enemy AcolyteOfUmordhoth Cards.acolyteOfUmordhoth
    & setPrey FewestCardsInHand

instance HasModifiersFor AcolyteOfUmordhoth where
  getModifiersFor (AcolyteOfUmordhoth a) = do
    anyWithoutCards <- selectAny (investigatorEngagedWith a <> HandWith NoCards)
    modifySelf a [CannotBeEvaded | anyWithoutCards]

instance RunMessage AcolyteOfUmordhoth where
  runMessage msg (AcolyteOfUmordhoth attrs) = AcolyteOfUmordhoth <$> runMessage msg attrs
