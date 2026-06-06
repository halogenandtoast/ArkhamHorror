module Arkham.Act.Cards.BlackwatersBane (blackwatersBane) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Trait (Trait (Ooze))

newtype BlackwatersBane = BlackwatersBane ActAttrs
  deriving anyclass (IsAct, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blackwatersBane :: ActCard BlackwatersBane
blackwatersBane = act (3, A) BlackwatersBane Cards.blackwatersBane Nothing

instance HasModifiersFor BlackwatersBane where
  getModifiersFor (BlackwatersBane a) =
    modifySelect a (EnemyWithTrait Ooze) [AddKeyword Keyword.Retaliate, ScenarioModifier "noBlob"]

instance RunMessage BlackwatersBane where
  runMessage msg (BlackwatersBane attrs) = BlackwatersBane <$> runMessage msg attrs
