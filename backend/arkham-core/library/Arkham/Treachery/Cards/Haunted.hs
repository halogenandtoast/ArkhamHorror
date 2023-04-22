module Arkham.Treachery.Cards.Haunted
  ( Haunted(..)
  , haunted
  ) where

import Arkham.Prelude

import Arkham.Ability hiding (Haunted, haunted)
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Classes
import Arkham.Message
import Arkham.Modifier
import Arkham.Treachery.Runner
import Arkham.Treachery.Helpers

newtype Haunted = Haunted TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

haunted :: TreacheryCard Haunted
haunted = treachery Haunted Cards.haunted

instance HasModifiersFor Haunted where
  getModifiersFor (InvestigatorTarget iid) (Haunted attrs) =
    pure $ toModifiers
      attrs
      [ AnySkillValue (-1) | treacheryOnInvestigator iid attrs ]
  getModifiersFor _ _ = pure []

instance HasAbilities Haunted where
  getAbilities (Haunted a) =
    [ restrictedAbility a 1 OnSameLocation $ ActionAbility Nothing $ ActionCost
        2
    ]

instance RunMessage Haunted where
  runMessage msg t@(Haunted attrs) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ push (AttachTreachery (toId attrs) $ InvestigatorTarget iid)
    UseCardAbility _ source 1 _ _ | isSource attrs source ->
      t <$ push (Discard (toAbilitySource attrs 1) $ toTarget attrs)
    _ -> Haunted <$> runMessage msg attrs
