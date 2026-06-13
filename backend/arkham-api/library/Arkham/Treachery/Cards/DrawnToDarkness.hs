module Arkham.Treachery.Cards.DrawnToDarkness (drawnToDarkness) where

import Arkham.Ability
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype DrawnToDarkness = DrawnToDarkness TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drawnToDarkness :: TreacheryCard DrawnToDarkness
drawnToDarkness = treachery DrawnToDarkness Cards.drawnToDarkness

instance HasModifiersFor DrawnToDarkness where
  getModifiersFor (DrawnToDarkness attrs) =
    inThreatAreaGets attrs [HandSize (-3), CheckHandSizeAfterDraw]

instance HasAbilities DrawnToDarkness where
  getAbilities (DrawnToDarkness a) =
    [restricted a 1 (InThreatAreaOf You <> youExist (HandWith NoCards)) $ forced $ TurnEnds #when You]

instance RunMessage DrawnToDarkness where
  runMessage msg t@(DrawnToDarkness attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      hasCopy <- selectAny $ treacheryIs Cards.drawnToDarkness <> treacheryInThreatAreaOf iid
      if hasCopy
        then gainSurge attrs
        else placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> DrawnToDarkness <$> liftRunMessage msg attrs
