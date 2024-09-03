module Arkham.Treachery.Cards.CurseOfTheRougarou (CurseOfTheRougarou (..), curseOfTheRougarou) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype CurseOfTheRougarou = CurseOfTheRougarou TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

curseOfTheRougarou :: TreacheryCard CurseOfTheRougarou
curseOfTheRougarou = treachery CurseOfTheRougarou Cards.curseOfTheRougarou

instance HasAbilities CurseOfTheRougarou where
  getAbilities (CurseOfTheRougarou x) =
    [ restrictedAbility x 1 (InThreatAreaOf You <> youExist NoDamageDealtThisTurn)
        $ forced
        $ TurnEnds #when You
    ]

instance RunMessage CurseOfTheRougarou where
  runMessage msg t@(CurseOfTheRougarou attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid (attrs.ability 1) 1
      pure t
    _ -> CurseOfTheRougarou <$> liftRunMessage msg attrs
