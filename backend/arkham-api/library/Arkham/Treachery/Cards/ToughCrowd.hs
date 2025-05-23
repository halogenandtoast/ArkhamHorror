module Arkham.Treachery.Cards.ToughCrowd (toughCrowd) where

import Arkham.Ability
import Arkham.Action
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Matcher
import Arkham.Message.Lifted.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ToughCrowd = ToughCrowd TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

toughCrowd :: TreacheryCard ToughCrowd
toughCrowd = treachery ToughCrowd Cards.toughCrowd

instance HasModifiersFor ToughCrowd where
  getModifiersFor (ToughCrowd a) = modifySelect a Anyone [AdditionalActionCostOf (IsAction Parley) 1]

instance HasAbilities ToughCrowd where
  getAbilities (ToughCrowd a) = [mkAbility a 1 $ forced $ RoundEnds #when]

instance RunMessage ToughCrowd where
  runMessage msg t@(ToughCrowd attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      place attrs NextToAgenda
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      toDiscard (attrs.ability 1) attrs
      pure t
    _ -> ToughCrowd <$> liftRunMessage msg attrs
