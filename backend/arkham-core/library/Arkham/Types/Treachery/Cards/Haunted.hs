module Arkham.Types.Treachery.Cards.Haunted
  ( Haunted(..)
  , haunted
  ) where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Helpers
import Arkham.Types.Treachery.Runner

newtype Haunted = Haunted TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

haunted :: TreacheryCard Haunted
haunted = treachery Haunted Cards.haunted

instance HasModifiersFor env Haunted where
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

instance TreacheryRunner env => RunMessage env Haunted where
  runMessage msg t@(Haunted attrs) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ push (AttachTreachery (toId attrs) $ InvestigatorTarget iid)
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      t <$ push (Discard $ toTarget attrs)
    _ -> Haunted <$> runMessage msg attrs
