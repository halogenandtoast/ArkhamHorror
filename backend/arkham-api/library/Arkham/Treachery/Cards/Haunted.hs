module Arkham.Treachery.Cards.Haunted (Haunted (..), haunted) where

import Arkham.Ability hiding (Haunted, haunted)
import Arkham.Classes
import Arkham.Modifier
import Arkham.Prelude
import Arkham.SkillType
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Helpers
import Arkham.Treachery.Runner

newtype Haunted = Haunted TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

haunted :: TreacheryCard Haunted
haunted = treachery Haunted Cards.haunted

instance HasModifiersFor Haunted where
  getModifiersFor (Haunted attrs) =
    inThreatAreaGets attrs [SkillModifier skillType (-1) | skillType <- allSkills]

instance HasAbilities Haunted where
  getAbilities (Haunted a) = [restrictedAbility a 1 OnSameLocation $ ActionAbility [] $ ActionCost 2]

instance RunMessage Haunted where
  runMessage msg t@(Haunted attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> Haunted <$> runMessage msg attrs
