module Arkham.Location.Cards.PorteDeLAvancee (porteDeLAvancee) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype PorteDeLAvancee = PorteDeLAvancee LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

porteDeLAvancee :: LocationCard PorteDeLAvancee
porteDeLAvancee = location PorteDeLAvancee Cards.porteDeLAvancee 3 (PerPlayer 1)

instance HasAbilities PorteDeLAvancee where
  getAbilities (PorteDeLAvancee a) =
    extendRevealed1 a $ restricted a 1 (Here <> AgendaExists AgendaWithAnyDoom) doubleActionAbility

instance RunMessage PorteDeLAvancee where
  runMessage msg l@(PorteDeLAvancee attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      agendas <- select AgendaWithAnyDoom
      agendasWithOtherAgendas <- forToSnd agendas (selectJust . NotAgenda . AgendaWithId)
      chooseOrRunOneM iid do
        for_ agendasWithOtherAgendas \(target, otherTarget) -> targeting target do
          removeDoom (attrs.ability 1) target 1
          placeDoom (attrs.ability 1) otherTarget 1
          placeTokens (attrs.ability 1) attrs #clue 2
      pure l
    _ -> PorteDeLAvancee <$> liftRunMessage msg attrs
