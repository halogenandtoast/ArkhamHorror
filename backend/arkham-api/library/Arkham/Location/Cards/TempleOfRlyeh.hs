module Arkham.Location.Cards.TempleOfRlyeh (templeOfRlyeh) where

import Arkham.Ability
import Arkham.Draw.Types
import Arkham.Keyword
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted hiding (Surge)
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Modifier

newtype TempleOfRlyeh = TempleOfRlyeh LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

templeOfRlyeh :: LocationCard TempleOfRlyeh
templeOfRlyeh = location TempleOfRlyeh Cards.templeOfRlyeh 3 (PerPlayer 2)

instance HasAbilities TempleOfRlyeh where
  getAbilities (TempleOfRlyeh x) =
    extendRevealed1 x
      $ skillTestAbility
      $ restricted x 1 Here
      $ forced
      $ RevealLocation #after You (be x)

instance RunMessage TempleOfRlyeh where
  runMessage msg l@(TempleOfRlyeh attrs) = runQueueT $ case msg of
    Msg.RevealLocation _ (is attrs -> True) -> do
      TempleOfRlyeh <$> liftRunMessage msg (attrs & labelL .~ "templeOfRlyeh")
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 3)
      pure l
    FailedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n -> do
      if n >= 3
        then drawEncounterCardEdit iid attrs \d ->
          d {cardDrawRules = singleton $ WithDrawnCardModifiers (toSource attrs) [AddKeyword Surge]}
        else drawEncounterCard iid attrs
      pure l
    _ -> TempleOfRlyeh <$> liftRunMessage msg attrs
