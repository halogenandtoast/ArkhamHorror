module Arkham.Treachery.Cards.DownAndOut (downAndOut) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype DownAndOut = DownAndOut TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

downAndOut :: TreacheryCard DownAndOut
downAndOut = treachery DownAndOut Cards.downAndOut

instance HasAbilities DownAndOut where
  getAbilities (DownAndOut a) =
    [ restricted
        a
        1
        (InThreatAreaOf You <> youExist (not_ (TakenActionThisRound $ oneOf [#draw, #resource])))
        $ forced
        $ TurnEnds #when You
    , restricted a 2 OnSameLocation $ actionAbilityWithCost (OrCost [ResourceCost 3, HandDiscardCost 3 #any])
    ]

instance RunMessage DownAndOut where
  runMessage msg t@(DownAndOut attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid (attrs.ability 1) 1
      shuffleIntoDeck iid attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> DownAndOut <$> liftRunMessage msg attrs
