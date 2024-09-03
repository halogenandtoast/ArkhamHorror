module Arkham.Treachery.Cards.UnhallowedCountry (UnhallowedCountry (..), unhallowedCountry) where

import Arkham.Ability
import Arkham.Asset.Types (Field (..))
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Projection
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype UnhallowedCountry = UnhallowedCountry TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unhallowedCountry :: TreacheryCard UnhallowedCountry
unhallowedCountry = treachery UnhallowedCountry Cards.unhallowedCountry

instance HasModifiersFor UnhallowedCountry where
  getModifiersFor (InvestigatorTarget iid) (UnhallowedCountry attrs) =
    modified attrs [CannotPlay (#asset <> #ally) | treacheryInThreatArea iid attrs]
  getModifiersFor (AssetTarget aid) (UnhallowedCountry attrs) = do
    isAlly <- fieldMap AssetTraits (member Ally) aid
    miid <- selectAssetController aid
    modified attrs $ case miid of
      Just iid -> [Blank | treacheryInThreatArea iid attrs && isAlly]
      Nothing -> []
  getModifiersFor _ _ = pure []

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
