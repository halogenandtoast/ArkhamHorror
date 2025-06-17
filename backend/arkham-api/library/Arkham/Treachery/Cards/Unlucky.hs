module Arkham.Treachery.Cards.Unlucky (unlucky) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Matcher
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Unlucky = Unlucky TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unlucky :: TreacheryCard Unlucky
unlucky = treachery Unlucky Cards.unlucky

instance HasModifiersFor Unlucky where
  getModifiersFor (Unlucky attrs) = case attrs.placement of
    InThreatArea iid -> modified_ attrs iid [AnySkillValue (-2)]
    _ -> pure mempty

instance HasAbilities Unlucky where
  getAbilities (Unlucky a) =
    [restricted a 1 (InThreatAreaOf You) $ forced $ RoundEnds #when]

instance RunMessage Unlucky where
  runMessage msg t@(Unlucky attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> Unlucky <$> liftRunMessage msg attrs
