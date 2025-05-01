module Arkham.Treachery.Cards.CursedLuck (cursedLuck) where

import Arkham.Ability
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype CursedLuck = CursedLuck TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cursedLuck :: TreacheryCard CursedLuck
cursedLuck = treachery CursedLuck Cards.cursedLuck

instance HasModifiersFor CursedLuck where
  getModifiersFor (CursedLuck attrs) = whenM (isJust <$> getSkillTest) do
    inThreatAreaGets attrs [AnySkillValue (-1)]

instance HasAbilities CursedLuck where
  getAbilities (CursedLuck x) =
    [ restricted x 1 (InThreatAreaOf You)
        $ forced
        $ SkillTestResult #after You AnySkillTest (SuccessResult $ atLeast 1)
    ]

instance RunMessage CursedLuck where
  runMessage msg t@(CursedLuck attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> CursedLuck <$> liftRunMessage msg attrs
