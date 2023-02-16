module Arkham.Treachery.Cards.LockedDoor
  ( LockedDoor(..)
  , lockedDoor
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.Message
import Arkham.Modifier
import Arkham.SkillType
import Arkham.Target
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Helpers
import Arkham.Treachery.Runner

newtype LockedDoor = LockedDoor TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lockedDoor :: TreacheryCard LockedDoor
lockedDoor = treachery LockedDoor Cards.lockedDoor

instance HasModifiersFor LockedDoor where
  getModifiersFor (LocationTarget lid) (LockedDoor attrs) = pure
    $ toModifiers attrs [ CannotInvestigate | treacheryOnLocation lid attrs ]
  getModifiersFor _ _ = pure []

instance HasAbilities LockedDoor where
  getAbilities (LockedDoor a) =
    [ restrictedAbility a 1 OnSameLocation $ ActionAbility Nothing $ ActionCost
        1
    ]

instance RunMessage LockedDoor where
  runMessage msg t@(LockedDoor attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      targets <-
        selectList
        $ LocationWithMostClues
        $ LocationWithoutTreachery
        $ treacheryIs Cards.lockedDoor
      t <$ case targets of
        [] -> pure ()
        [x] -> pushAll [AttachTreachery treacheryId (LocationTarget x)]
        xs -> push $ chooseOne
          iid
          [ targetLabel x [AttachTreachery treacheryId (LocationTarget x)]
          | x <- xs
          ]
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      let
        target = toTarget attrs
        chooseSkillTest sType =
          SkillLabel sType [beginSkillTest iid source target sType 4]
      t <$ push (chooseOne iid $ map chooseSkillTest [SkillCombat, SkillAgility])
    PassedSkillTest _ _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> t <$ push (Discard (toSource attrs) $ toTarget attrs)
    _ -> LockedDoor <$> runMessage msg attrs
