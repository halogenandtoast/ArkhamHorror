module Arkham.Homebrew.CircusExMortis.Locations.MoonlitForestGlassyLake (
  moonlitForestGlassyLake,
) where

import Arkham.Ability
import Arkham.ChaosToken.Types (ChaosTokenFace (MoonToken))
import Arkham.Helpers.ChaosBag (getBagChaosTokens)
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Token (Token (..))
import Arkham.Trait (Trait (Woods))

newtype MoonlitForestGlassyLake = MoonlitForestGlassyLake LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moonlitForestGlassyLake :: LocationCard MoonlitForestGlassyLake
moonlitForestGlassyLake =
  locationWith
    MoonlitForestGlassyLake
    Cards.moonlitForestGlassyLake
    1
    (Static 1)
    connectsToAdjacent

instance HasAbilities MoonlitForestGlassyLake where
  getAbilities (MoonlitForestGlassyLake a) =
    extendRevealed
      a
      [ mkAbility a 1 $ forced $ RevealLocation #after You (be a)
      , -- TODO(homebrew): printed as an additional cost to move; modeled as an after-move
        -- forced effect rather than a pre-move payment.
        mkAbility a 2 $ forced $ Moves #after You AnySource (be a) (LocationWithTrait Woods)
      ]

instance RunMessage MoonlitForestGlassyLake where
  runMessage msg l@(MoonlitForestGlassyLake attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectOne (locationIs Cards.woodlandOverlook)
        >>= traverse_ \lid -> placeTokens (attrs.ability 1) lid Damage 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      -- "search the token bag for a moon token and seal it on your investigator card."
      moons <- filter ((== MoonToken) . (.face)) <$> getBagChaosTokens
      for_ (listToMaybe moons) \token -> sealChaosToken iid iid token
      pure l
    _ -> MoonlitForestGlassyLake <$> liftRunMessage msg attrs
