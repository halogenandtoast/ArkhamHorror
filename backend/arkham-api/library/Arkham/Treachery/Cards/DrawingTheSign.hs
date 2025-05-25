module Arkham.Treachery.Cards.DrawingTheSign (drawingTheSign) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (HandSize), inThreatAreaGets)
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype DrawingTheSign = DrawingTheSign TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drawingTheSign :: TreacheryCard DrawingTheSign
drawingTheSign = treachery DrawingTheSign Cards.drawingTheSign

instance HasModifiersFor DrawingTheSign where
  getModifiersFor (DrawingTheSign attrs) = inThreatAreaGets attrs [HandSize (-5)]

instance HasAbilities DrawingTheSign where
  getAbilities (DrawingTheSign a) = [restricted a 1 OnSameLocation doubleActionAbility]

instance RunMessage DrawingTheSign where
  runMessage msg t@(DrawingTheSign attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (toAbilitySource attrs 1) attrs
      pure t
    _ -> DrawingTheSign <$> liftRunMessage msg attrs
