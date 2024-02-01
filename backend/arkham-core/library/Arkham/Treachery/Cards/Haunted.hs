module Arkham.Treachery.Cards.Haunted (
  Haunted (..),
  haunted,
) where

import Arkham.Prelude

import Arkham.Ability hiding (Haunted, haunted)
import Arkham.Classes
import Arkham.Modifier
import Arkham.SkillType
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Helpers
import Arkham.Treachery.Runner

newtype Haunted = Haunted TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

haunted :: TreacheryCard Haunted
haunted = treachery Haunted Cards.haunted

instance HasModifiersFor Haunted where
  getModifiersFor (InvestigatorTarget iid) (Haunted attrs) =
    pure
      $ toModifiers attrs
      $ [SkillModifier skillType (-1) | treacheryOnInvestigator iid attrs, skillType <- allSkills]
  getModifiersFor _ _ = pure []

instance HasAbilities Haunted where
  getAbilities (Haunted a) =
    [ restrictedAbility a 1 OnSameLocation
        $ ActionAbility []
        $ ActionCost 2
    ]

instance RunMessage Haunted where
  runMessage msg t@(Haunted attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ AttachTreachery (toId attrs) (toTarget iid)
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ toDiscardBy iid (toAbilitySource attrs 1) attrs
      pure t
    _ -> Haunted <$> runMessage msg attrs
