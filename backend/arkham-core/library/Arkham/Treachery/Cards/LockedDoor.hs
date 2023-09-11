module Arkham.Treachery.Cards.LockedDoor (
  LockedDoor (..),
  lockedDoor,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Matcher
import Arkham.Message
import Arkham.Modifier
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Helpers
import Arkham.Treachery.Runner

newtype LockedDoor = LockedDoor TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lockedDoor :: TreacheryCard LockedDoor
lockedDoor = treachery LockedDoor Cards.lockedDoor

instance HasModifiersFor LockedDoor where
  getModifiersFor (LocationTarget lid) (LockedDoor attrs) =
    pure $ toModifiers attrs [CannotInvestigate | treacheryOnLocation lid attrs]
  getModifiersFor _ _ = pure []

instance HasAbilities LockedDoor where
  getAbilities (LockedDoor a) =
    [restrictedAbility a 1 OnSameLocation (ActionAbility Nothing $ ActionCost 1)]

instance RunMessage LockedDoor where
  runMessage msg t@(LockedDoor attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      targets <-
        selectList $ LocationWithMostClues $ LocationWithoutTreachery $ treacheryIs Cards.lockedDoor
      case targets of
        [] -> pure ()
        [x] -> pushAll [AttachTreachery treacheryId (LocationTarget x)]
        xs ->
          push
            $ chooseOne iid
            $ [ targetLabel x [AttachTreachery treacheryId (LocationTarget x)]
              | x <- xs
              ]
      pure t
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      let chooseSkillTest sType = SkillLabel sType [beginSkillTest iid (toAbilitySource attrs 1) attrs sType 4]
      push $ chooseOne iid $ map chooseSkillTest [#combat, #agility]
      pure t
    PassedSkillTest _ _ (isAbilitySource attrs 1 -> True) SkillTestInitiatorTarget {} _ _ -> do
      push $ Discard (toAbilitySource attrs 1) (toTarget attrs)
      pure t
    _ -> LockedDoor <$> runMessage msg attrs
