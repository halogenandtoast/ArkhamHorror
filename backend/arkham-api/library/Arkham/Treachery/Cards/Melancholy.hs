module Arkham.Treachery.Cards.Melancholy (melancholy) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Matcher
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Melancholy = Melancholy TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

melancholy :: TreacheryCard Melancholy
melancholy = treachery Melancholy Cards.melancholy

instance HasModifiersFor Melancholy where
  getModifiersFor (Melancholy a) = case a.placement of
    InThreatArea iid -> modified_ a iid [IncreaseCostOf (inHandOf ForPlay iid) 1]
    _ -> pure ()

instance HasAbilities Melancholy where
  getAbilities (Melancholy a) =
    [skillTestAbility $ restricted a 1 (InThreatAreaOf You) $ forced $ TurnEnds #when You]

instance RunMessage Melancholy where
  runMessage msg t@(Melancholy attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 3)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> Melancholy <$> liftRunMessage msg attrs
