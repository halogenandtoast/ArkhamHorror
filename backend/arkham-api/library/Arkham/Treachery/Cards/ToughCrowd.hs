module Arkham.Treachery.Cards.ToughCrowd (toughCrowd, ToughCrowd (..)) where

import Arkham.Ability
import Arkham.Action
import Arkham.Classes
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Prelude
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Helpers
import Arkham.Treachery.Runner

newtype ToughCrowd = ToughCrowd TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

toughCrowd :: TreacheryCard ToughCrowd
toughCrowd = treachery ToughCrowd Cards.toughCrowd

instance HasModifiersFor ToughCrowd where
  getModifiersFor (InvestigatorTarget _) (ToughCrowd a) =
    pure $ toModifiers a [AdditionalActionCostOf (IsAction Parley) 1]
  getModifiersFor _ _ = pure []

instance HasAbilities ToughCrowd where
  getAbilities (ToughCrowd a) = [mkAbility a 1 $ forced $ RoundEnds #when]

instance RunMessage ToughCrowd where
  runMessage msg t@(ToughCrowd attrs) = case msg of
    Revelation _ (isSource attrs -> True) -> do
      push $ PlaceTreachery (toId attrs) NextToAgenda
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ toDiscard (attrs.ability 1) attrs
      pure t
    _ -> ToughCrowd <$> runMessage msg attrs
