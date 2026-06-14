module Arkham.Location.Cards.ChurningChasm (churningChasm) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype ChurningChasm = ChurningChasm LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

churningChasm :: LocationCard ChurningChasm
churningChasm = location ChurningChasm Cards.churningChasm 2 (Static 1)

instance HasAbilities ChurningChasm where
  getAbilities (ChurningChasm a) =
    extendRevealed1 a $ restricted a 1 Here (freeReaction $ TurnBegins #when You)

instance RunMessage ChurningChasm where
  runMessage msg l@(ChurningChasm attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      squamousParasite <- getSetAsideCard Enemies.squamousParasite
      createEnemyAt_ squamousParasite attrs
      pure l
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseSelectM iid (RevealedLocation <> not_ (be attrs)) $ moveTo (attrs.ability 1) iid
      pure l
    _ -> ChurningChasm <$> liftRunMessage msg attrs
