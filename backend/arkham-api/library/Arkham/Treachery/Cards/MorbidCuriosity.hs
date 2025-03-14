module Arkham.Treachery.Cards.MorbidCuriosity (morbidCuriosity) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype MorbidCuriosity = MorbidCuriosity TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

morbidCuriosity :: TreacheryCard MorbidCuriosity
morbidCuriosity = treachery MorbidCuriosity Cards.morbidCuriosity

instance HasAbilities MorbidCuriosity where
  getAbilities (MorbidCuriosity a) =
    [ restricted
        a
        1
        (InThreatAreaOf You <> youExist (not_ SuccessfullyInvestigatedThisRound))
        $ forced
        $ TurnEnds #when You
    , restricted a 2 OnSameLocation investigateAction_
    ]

instance RunMessage MorbidCuriosity where
  runMessage msg t@(MorbidCuriosity attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid (attrs.ability 1) 1
      shuffleIntoDeck iid attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      skillTestModifier sid (attrs.ability 2) iid (AnySkillValue (-2))
      investigate sid iid (attrs.ability 2)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> MorbidCuriosity <$> liftRunMessage msg attrs
