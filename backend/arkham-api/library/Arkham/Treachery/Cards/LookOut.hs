module Arkham.Treachery.Cards.LookOut (lookOut) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype LookOut = LookOut TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lookOut :: TreacheryCard LookOut
lookOut = treachery LookOut Cards.lookOut

instance HasAbilities LookOut where
  getAbilities (LookOut a) =
    [ groupLimit PerRound
        $ restricted a 1 (InThreatAreaOf You)
        $ forced
        $ Enters #after You FloodedLocation
    , skillTestAbility $ restricted a 2 (InThreatAreaOf You) $ forced $ TurnEnds #when You
    ]

instance RunMessage LookOut where
  runMessage msg t@(LookOut attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #intellect (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid (attrs.ability 1) 1
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 2) iid #agility (Fixed 3)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> LookOut <$> liftRunMessage msg attrs
