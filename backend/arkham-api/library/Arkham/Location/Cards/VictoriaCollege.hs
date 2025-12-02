module Arkham.Location.Cards.VictoriaCollege (victoriaCollege) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Strategy

newtype VictoriaCollege = VictoriaCollege LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

victoriaCollege :: LocationCard VictoriaCollege
victoriaCollege = symbolLabel $ location VictoriaCollege Cards.victoriaCollege 0 (Static 0)

-- TODO: Needs to be exactly 1 card
instance HasAbilities VictoriaCollege where
  getAbilities (VictoriaCollege a) =
    extendRevealed1 a
      $ playerLimit PerRound
      $ restricted a 1 Here
      $ freeReaction
      $ WouldDrawExactlyOneCard #when You (DeckOf You)

instance RunMessage VictoriaCollege where
  runMessage msg l@(VictoriaCollege attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      msgs <- capture $ lookAt iid (attrs.ability 1) iid [fromTopOfDeck 2] #any (DrawFound iid 1)
      push $ Instead (DoDrawCards iid) (Run msgs)
      pure l
    _ -> VictoriaCollege <$> liftRunMessage msg attrs
