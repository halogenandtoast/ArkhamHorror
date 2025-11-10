module Arkham.Location.Cards.ElMalecon (elMalecon) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype ElMalecon = ElMalecon LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

elMalecon :: LocationCard ElMalecon
elMalecon = symbolLabel $ location ElMalecon Cards.elMalecon 5 (PerPlayer 1)

instance HasAbilities ElMalecon where
  getAbilities (ElMalecon a) =
    extendRevealed1 a
      $ groupLimit PerRound
      $ restricted a 1 (Here <> exists (ConcealedCardAt (be a)))
      $ freeReaction
      $ CampaignEvent #after (Just You) "exposed[decoy]"

instance RunMessage ElMalecon where
  runMessage msg l@(ElMalecon attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      concealed <- map toId <$> getConcealedAt (ForExpose $ toSource attrs) attrs.id
      chooseTargetM iid concealed (exposeConcealed iid (attrs.ability 1))
      pure l
    _ -> ElMalecon <$> liftRunMessage msg attrs
