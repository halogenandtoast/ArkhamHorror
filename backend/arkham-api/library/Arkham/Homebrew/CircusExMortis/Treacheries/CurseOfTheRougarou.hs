module Arkham.Homebrew.CircusExMortis.Treacheries.CurseOfTheRougarou (curseOfTheRougarou) where

import Arkham.Ability
import Arkham.Homebrew.CircusExMortis.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (moonToken)
import Arkham.Matcher
import Arkham.Treachery.Import.Lifted

newtype CurseOfTheRougarou = CurseOfTheRougarou TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

curseOfTheRougarou :: TreacheryCard CurseOfTheRougarou
curseOfTheRougarou = treachery CurseOfTheRougarou Cards.curseOfTheRougarou

instance HasAbilities CurseOfTheRougarou where
  getAbilities (CurseOfTheRougarou x) =
    [ restricted x 1 (InThreatAreaOf You <> youExist NoDamageDealtThisTurn)
        $ forced
        $ TurnEnds #when You
    , restricted x 2 (InThreatAreaOf You)
        $ freeReaction (ChaosTokenReleased #after You moonToken)
    ]

instance RunMessage CurseOfTheRougarou where
  runMessage msg t@(CurseOfTheRougarou attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid (attrs.ability 1) 1
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> CurseOfTheRougarou <$> liftRunMessage msg attrs
