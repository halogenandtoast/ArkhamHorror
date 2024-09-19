module Arkham.Location.Cards.MiskatonicUniversity (MiskatonicUniversity (..), miskatonicUniversity) where

import Arkham.Ability
import Arkham.Capability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (miskatonicUniversity)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Strategy

newtype MiskatonicUniversity = MiskatonicUniversity LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

miskatonicUniversity :: LocationCard MiskatonicUniversity
miskatonicUniversity = location MiskatonicUniversity Cards.miskatonicUniversity 4 (PerPlayer 2)

instance HasAbilities MiskatonicUniversity where
  getAbilities (MiskatonicUniversity x) =
    extendRevealed1 x $ restricted x 1 (Here <> can.search.deck You) actionAbility

instance RunMessage MiskatonicUniversity where
  runMessage msg l@(MiskatonicUniversity attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      search iid (attrs.ability 1) iid [fromTopOfDeck 6] (basic $ oneOf [#tome, #spell]) (DrawFound iid 1)
      pure l
    _ -> MiskatonicUniversity <$> liftRunMessage msg attrs
