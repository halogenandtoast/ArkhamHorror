module Arkham.Location.Cards.SewerTunnelsToxicWastePit (sewerTunnelsToxicWastePit) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Message.Discard (chooseAndDiscardCard)
import Arkham.Location.Cards qualified as Cards (sewerTunnelsToxicWastePit)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message (toMessage)

newtype SewerTunnelsToxicWastePit = SewerTunnelsToxicWastePit LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sewerTunnelsToxicWastePit :: LocationCard SewerTunnelsToxicWastePit
sewerTunnelsToxicWastePit = location SewerTunnelsToxicWastePit Cards.sewerTunnelsToxicWastePit 3 (PerPlayer 1)

instance HasAbilities SewerTunnelsToxicWastePit where
  getAbilities (SewerTunnelsToxicWastePit a) =
    extendRevealed1 a $ restricted a 1 Here $ forced $ TurnEnds #when You

instance RunMessage SewerTunnelsToxicWastePit where
  runMessage msg l@(SewerTunnelsToxicWastePit attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ toMessage $ chooseAndDiscardCard iid (attrs.ability 1)
      pure l
    _ -> SewerTunnelsToxicWastePit <$> liftRunMessage msg attrs
