module Arkham.Location.Cards.MoonlitForestShadowedPathCircusExMortis (
  moonlitForestShadowedPathCircusExMortis,
) where

import Arkham.Ability
import Arkham.Campaigns.CircusExMortis.Helpers (getSealedMoonTokens)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype MoonlitForestShadowedPathCircusExMortis = MoonlitForestShadowedPathCircusExMortis LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moonlitForestShadowedPathCircusExMortis :: LocationCard MoonlitForestShadowedPathCircusExMortis
moonlitForestShadowedPathCircusExMortis =
  locationWith
    MoonlitForestShadowedPathCircusExMortis
    Cards.moonlitForestShadowedPathCircusExMortis
    3
    (Static 2)
    connectsToAdjacent

instance HasAbilities MoonlitForestShadowedPathCircusExMortis where
  getAbilities (MoonlitForestShadowedPathCircusExMortis a) =
    -- "This location and each adjacent copy of Moonlit Forest gain 'Forced - After you end
    -- your turn at this location, if there are no moon tokens sealed on your investigator
    -- card: Take 1 horror.'" Modeled as one forced ability whose window covers ending your
    -- turn at Shadowed Path or an adjacent Moonlit Forest.
    extendRevealed1 a
      $ restricted
        a
        1
        (youExist $ at_ (oneOf [be a, LocationWithTitle "Moonlit Forest" <> connectedTo (be a)]))
      $ forced
      $ TurnEnds #after You

instance RunMessage MoonlitForestShadowedPathCircusExMortis where
  runMessage msg l@(MoonlitForestShadowedPathCircusExMortis attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      moons <- getSealedMoonTokens iid
      when (null moons) $ assignHorror iid (attrs.ability 1) 1
      pure l
    _ -> MoonlitForestShadowedPathCircusExMortis <$> liftRunMessage msg attrs
