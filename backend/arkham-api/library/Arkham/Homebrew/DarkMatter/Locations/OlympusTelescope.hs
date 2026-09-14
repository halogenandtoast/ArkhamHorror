module Arkham.Homebrew.DarkMatter.Locations.OlympusTelescope (olympusTelescope) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Modifiers (ModifierType (AsIfAt))
import Arkham.Helpers.SkillTest.Lifted (investigateLocation_)
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype OlympusTelescope = OlympusTelescope LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

olympusTelescope :: LocationCard OlympusTelescope
olympusTelescope = symbolLabel $ location OlympusTelescope Cards.olympusTelescope 2 (PerPlayer 1)

{- | "[action][action]: Investigate. Choose any revealed location. Investigate as
if you were at that location."

The investigation is built from the /chosen/ location, so it uses that location's
shroud and discovers its clues; 'AsIfAt' carries "as if you were at that
location" over to everything else that reads your location during the test (see
'Arkham.Asset.Assets.PocketTelescope', and #5480).
-}
instance HasAbilities OlympusTelescope where
  getAbilities (OlympusTelescope a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> exists (RevealedLocation <> InvestigatableLocation))
      $ ActionAbility #investigate #intellect (ActionCost 2)

instance RunMessage OlympusTelescope where
  runMessage msg l@(OlympusTelescope attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <- select $ RevealedLocation <> InvestigatableLocation
      chooseHandleTargetM iid (attrs.ability 1) locations
      pure l
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (LocationTarget lid) -> do
      abilityModifier (AbilityRef (toSource attrs) 1) (attrs.ability 1) iid (AsIfAt lid)
      sid <- getRandom
      investigateLocation_ sid iid (attrs.ability 1) lid
      pure l
    _ -> OlympusTelescope <$> liftRunMessage msg attrs
