module Arkham.Homebrew.CircusExMortis.Locations.MoonlitForestGlassyLake (moonlitForestGlassyLake) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (moonToken)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Token (Token (..))
import Arkham.Trait (Trait (Woods))

newtype MoonlitForestGlassyLake = MoonlitForestGlassyLake LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moonlitForestGlassyLake :: LocationCard MoonlitForestGlassyLake
moonlitForestGlassyLake =
  location
    MoonlitForestGlassyLake
    Cards.moonlitForestGlassyLake
    1
    (Static 1)

instance HasModifiersFor MoonlitForestGlassyLake where
  getModifiersFor (MoonlitForestGlassyLake a) =
    modifySelect
      a
      (InvestigatorAt $ be a)
      [AdditionalCostToEnterMatching (LocationWithTrait Woods) $ SealOnInvestigatorCost moonToken]

instance HasAbilities MoonlitForestGlassyLake where
  getAbilities (MoonlitForestGlassyLake a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ RevealLocation #after You (be a)

instance RunMessage MoonlitForestGlassyLake where
  runMessage msg l@(MoonlitForestGlassyLake attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectOne (locationIs Cards.woodlandOverlook)
        >>= traverse_ \lid -> placeTokens (attrs.ability 1) lid Damage 1
      pure l
    _ -> MoonlitForestGlassyLake <$> liftRunMessage msg attrs
