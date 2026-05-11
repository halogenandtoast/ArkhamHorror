module Arkham.Treachery.Cards.Desiccation (desiccation) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectMap)
import Arkham.Matcher
import Arkham.Message.Lifted.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Desiccation = Desiccation TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

desiccation :: TreacheryCard Desiccation
desiccation = treachery Desiccation Cards.desiccation

instance HasModifiersFor Desiccation where
  getModifiersFor (Desiccation a) =
    modifySelectMap a Anyone \iid ->
      [AdditionalPlayCostOf (basic AnyCard) (DamageCost (toSource a) (toTarget iid) 1)]

instance HasAbilities Desiccation where
  getAbilities (Desiccation a) =
    [limited (MaxPer Cards.desiccation PerRound 1) $ mkAbility a 1 $ forced $ RoundEnds #at]

instance RunMessage Desiccation where
  runMessage msg t@(Desiccation attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      place attrs NextToAgenda
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> Desiccation <$> liftRunMessage msg attrs
