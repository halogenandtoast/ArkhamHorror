module Arkham.Treachery.Cards.Haunted
  ( Haunted(..)
  , haunted
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Message
import Arkham.Modifier
import Arkham.Target
import Arkham.Treachery.Attrs
import Arkham.Treachery.Helpers
import Arkham.Treachery.Runner

newtype Haunted = Haunted TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

haunted :: TreacheryCard Haunted
haunted = treachery Haunted Cards.haunted

instance HasModifiersFor Haunted where
  getModifiersFor _ (InvestigatorTarget iid) (Haunted attrs) =
    pure $ toModifiers
      attrs
      [ AnySkillValue (-1) | treacheryOnInvestigator iid attrs ]
  getModifiersFor _ _ _ = pure []

instance HasAbilities Haunted where
  getAbilities (Haunted a) =
    [ restrictedAbility a 1 OnSameLocation $ ActionAbility Nothing $ ActionCost
        2
    ]

instance TreacheryRunner env => RunMessage Haunted where
  runMessage msg t@(Haunted attrs) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ push (AttachTreachery (toId attrs) $ InvestigatorTarget iid)
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      t <$ push (Discard $ toTarget attrs)
    _ -> Haunted <$> runMessage msg attrs
