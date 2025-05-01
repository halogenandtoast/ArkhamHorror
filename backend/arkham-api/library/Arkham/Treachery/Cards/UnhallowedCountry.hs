module Arkham.Treachery.Cards.UnhallowedCountry (unhallowedCountry) where

import Arkham.Ability
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype UnhallowedCountry = UnhallowedCountry TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unhallowedCountry :: TreacheryCard UnhallowedCountry
unhallowedCountry = treachery UnhallowedCountry Cards.unhallowedCountry

instance HasModifiersFor UnhallowedCountry where
  getModifiersFor (UnhallowedCountry attrs) = for_ attrs.placement.inThreatAreaOf \iid -> do
    inThreatAreaGets attrs [CannotPlay (#asset <> #ally)]
    modifySelect attrs (#ally <> assetControlledBy iid) [Blank]

instance HasAbilities UnhallowedCountry where
  getAbilities (UnhallowedCountry x) = [skillTestAbility $ restricted x 1 (InThreatAreaOf You) $ forced $ TurnEnds #when You]

instance RunMessage UnhallowedCountry where
  runMessage msg t@(UnhallowedCountry attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 3)
      pure t
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> UnhallowedCountry <$> liftRunMessage msg attrs
