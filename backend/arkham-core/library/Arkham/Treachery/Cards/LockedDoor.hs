module Arkham.Treachery.Cards.LockedDoor (
  LockedDoor (..),
  lockedDoor,
) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Prelude
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype LockedDoor = LockedDoor TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

lockedDoor :: TreacheryCard LockedDoor
lockedDoor = treachery LockedDoor Cards.lockedDoor

instance HasModifiersFor LockedDoor where
  getModifiersFor (LocationTarget lid) (LockedDoor attrs) =
    pure $ toModifiers attrs [CannotInvestigate | treacheryOnLocation lid attrs]
  getModifiersFor _ _ = pure []

instance HasAbilities LockedDoor where
  getAbilities (LockedDoor a) = [restrictedAbility a 1 OnSameLocation actionAbility]

instance RunMessage LockedDoor where
  runMessage msg t@(LockedDoor attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      locations <-
        selectList $ LocationWithMostClues $ LocationWithoutTreachery $ treacheryIs Cards.lockedDoor
      player <- getPlayer iid
      pushIfAny locations
        $ chooseOrRunOne player
        $ targetLabels locations (only . AttachTreachery treacheryId . toTarget)
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let chooseSkillTest sType = SkillLabel sType [beginSkillTest iid (toAbilitySource attrs 1) attrs sType 4]
      player <- getPlayer iid
      push $ chooseOne player $ map chooseSkillTest [#combat, #agility]
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      push $ toDiscardBy iid (toAbilitySource attrs 1) attrs
      pure t
    _ -> LockedDoor <$> runMessage msg attrs
