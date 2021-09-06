module Arkham.Types.Treachery.Cards.LockedDoor
  ( LockedDoor(..)
  , lockedDoor
  ) where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Helpers
import Arkham.Types.Treachery.Runner

newtype LockedDoor = LockedDoor TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lockedDoor :: TreacheryCard LockedDoor
lockedDoor = treachery LockedDoor Cards.lockedDoor

instance HasModifiersFor env LockedDoor where
  getModifiersFor _ (LocationTarget lid) (LockedDoor attrs) =
    pure $ toModifiers
      attrs
      [ CannotInvestigate | treacheryOnLocation lid attrs ]
  getModifiersFor _ _ _ = pure []

instance HasAbilities LockedDoor where
  getAbilities (LockedDoor a) =
    [ restrictedAbility a 1 OnSameLocation $ ActionAbility Nothing $ ActionCost
        1
    ]

instance (TreacheryRunner env) => RunMessage env LockedDoor where
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
        xs -> push
          (chooseOne
            iid
            [ AttachTreachery treacheryId (LocationTarget x) | x <- xs ]
          )
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      let
        target = toTarget attrs
        beginSkillTest sType = BeginSkillTest iid source target Nothing sType 4
      t <$ push (chooseOne iid $ map beginSkillTest [SkillCombat, SkillAgility])
    PassedSkillTest _ _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> t <$ push (Discard $ toTarget attrs)
    _ -> LockedDoor <$> runMessage msg attrs
