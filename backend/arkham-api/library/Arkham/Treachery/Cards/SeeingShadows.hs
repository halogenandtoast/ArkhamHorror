module Arkham.Treachery.Cards.SeeingShadows (seeingShadows) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype SeeingShadows = SeeingShadows TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

seeingShadows :: TreacheryCard SeeingShadows
seeingShadows = treachery SeeingShadows Cards.seeingShadows

instance HasAbilities SeeingShadows where
  getAbilities (SeeingShadows a) =
    [ restricted a 1 (InYourThreatArea <> youExist (at_ LocationWithConcealedCard))
        $ forced
        $ SkillTestResult #after You AnySkillTest #failure
    , restricted a 2 OnSameLocation doubleActionAbility
    ]

instance RunMessage SeeingShadows where
  runMessage msg t@(SeeingShadows attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid attrs 1
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> SeeingShadows <$> liftRunMessage msg attrs
