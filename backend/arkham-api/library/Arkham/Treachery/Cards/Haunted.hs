module Arkham.Treachery.Cards.Haunted (haunted) where

import Arkham.Ability hiding (Haunted, haunted)
import Arkham.Helpers.Modifiers (ModifierType (..), inThreatAreaGets)
import Arkham.SkillType
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Haunted = Haunted TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

haunted :: TreacheryCard Haunted
haunted = treachery Haunted Cards.haunted

instance HasModifiersFor Haunted where
  getModifiersFor (Haunted attrs) =
    inThreatAreaGets attrs [SkillModifier skillType (-1) | skillType <- allSkills]

instance HasAbilities Haunted where
  getAbilities (Haunted a) = [restricted a 1 OnSameLocation doubleActionAbility]

instance RunMessage Haunted where
  runMessage msg t@(Haunted attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> Haunted <$> liftRunMessage msg attrs
