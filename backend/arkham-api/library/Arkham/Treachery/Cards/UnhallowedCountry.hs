module Arkham.Treachery.Cards.UnhallowedCountry (UnhallowedCountry (..), unhallowedCountry) where

import Arkham.Ability
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype UnhallowedCountry = UnhallowedCountry TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unhallowedCountry :: TreacheryCard UnhallowedCountry
unhallowedCountry = treachery UnhallowedCountry Cards.unhallowedCountry

instance HasModifiersFor UnhallowedCountry where
  getModifiersFor (UnhallowedCountry attrs) = case attrs.placement of
    InThreatArea iid -> do
      threat <- inThreatAreaGets attrs [CannotPlay (#asset <> #ally)]
      assets <- modifySelect attrs (#ally <> assetControlledBy iid) [Blank]
      pure $ threat <> assets
    _ -> pure mempty

instance HasAbilities UnhallowedCountry where
  getAbilities (UnhallowedCountry x) = [skillTestAbility $ restrictedAbility x 1 (InThreatAreaOf You) $ forced $ TurnEnds #when You]

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
