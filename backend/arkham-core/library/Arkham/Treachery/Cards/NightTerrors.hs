module Arkham.Treachery.Cards.NightTerrors (nightTerrors, NightTerrors (..)) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Strategy
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype NightTerrors = NightTerrors TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nightTerrors :: TreacheryCard NightTerrors
nightTerrors = treachery NightTerrors Cards.nightTerrors

instance HasAbilities NightTerrors where
  getAbilities (NightTerrors a) =
    [ restrictedAbility a 1 (InThreatAreaOf You) $ forced $ SkillTestResult #after You #any #failure
    , skillTestAbility $ restrictedAbility a 2 (InThreatAreaOf You) actionAbility
    ]

instance RunMessage NightTerrors where
  runMessage msg t@(NightTerrors attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      lookAt iid (attrs.ability 1) iid [(FromTopOfDeck 3, RemoveRestFromGame)] #weakness
        $ DrawAllFound iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 2) iid #willpower (Fixed 4)
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> NightTerrors <$> liftRunMessage msg attrs
