module Arkham.Homebrew.CircusExMortis.Locations.MoonlitForestShadowedPath (
  moonlitForestShadowedPath,
) where

import Arkham.Ability
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (campaignI18n, hasSealedMoonToken)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype MoonlitForestShadowedPath = MoonlitForestShadowedPath LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moonlitForestShadowedPath :: LocationCard MoonlitForestShadowedPath
moonlitForestShadowedPath =
  location
    MoonlitForestShadowedPath
    Cards.moonlitForestShadowedPath
    3
    (Static 2)

instance HasAbilities MoonlitForestShadowedPath where
  getAbilities (MoonlitForestShadowedPath a) =
    -- The ability is proxied onto the adjacent forests, where nothing else names its
    -- origin, so the tooltip says which location granted it.
    extendRevealed1 a
      $ campaignI18n
      $ withI18nTooltip "moonlitForestShadowedPath"
      $ restricted
        ( proxied
            ( LocationMatcherSource
                $ oneOf [be a, LocationWithTitle "Moonlit Forest" <> connectedTo (be a)]
            )
            a
        )
        1
        (Here <> youExist (not_ hasSealedMoonToken))
      $ forced
      $ TurnEnds #after You

instance RunMessage MoonlitForestShadowedPath where
  runMessage msg l@(MoonlitForestShadowedPath attrs) = runQueueT $ case msg of
    UseThisAbility iid (isProxySource attrs -> True) 1 -> do
      assignHorror iid (attrs.ability 1) 1
      pure l
    _ -> MoonlitForestShadowedPath <$> liftRunMessage msg attrs
