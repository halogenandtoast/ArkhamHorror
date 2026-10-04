module Arkham.Homebrew.TheSymphonyOfErichZann.Treacheries.ShriekingViolin (shriekingViolin) where

import Arkham.Ability
import Arkham.Helpers.Message.Discard.Lifted (randomDiscard)
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.TheSymphonyOfErichZann.Helpers (placeMusicTreachery)
import Arkham.Matcher
import Arkham.Treachery.Import.Lifted

newtype ShriekingViolin = ShriekingViolin TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shriekingViolin :: TreacheryCard ShriekingViolin
shriekingViolin = treachery ShriekingViolin Cards.shriekingViolin

instance HasAbilities ShriekingViolin where
  -- "At the end of your turn: Randomly discard 1 card from your hand. Then, draw 1 card."
  getAbilities (ShriekingViolin a) = [mkAbility a 1 $ forced $ TurnEnds #when Anyone]

instance RunMessage ShriekingViolin where
  runMessage msg t@(ShriekingViolin attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      placeMusicTreachery attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      randomDiscard iid (attrs.ability 1)
      drawCards iid (attrs.ability 1) 1
      pure t
    _ -> ShriekingViolin <$> liftRunMessage msg attrs
