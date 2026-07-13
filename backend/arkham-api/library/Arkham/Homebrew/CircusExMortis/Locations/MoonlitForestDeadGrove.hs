module Arkham.Homebrew.CircusExMortis.Locations.MoonlitForestDeadGrove (
  moonlitForestDeadGrove,
) where

import Arkham.Ability
import Arkham.ChaosToken.Types (ChaosTokenFace (MoonToken))
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted hiding (RevealChaosToken)
import Arkham.Matcher

newtype MoonlitForestDeadGrove = MoonlitForestDeadGrove LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moonlitForestDeadGrove :: LocationCard MoonlitForestDeadGrove
moonlitForestDeadGrove =
  locationWith
    MoonlitForestDeadGrove
    Cards.moonlitForestDeadGrove
    3
    (Static 2)
    connectsToAdjacent

instance HasAbilities MoonlitForestDeadGrove where
  getAbilities (MoonlitForestDeadGrove a) =
    -- "Each moon token revealed at this location and each adjacent copy of Moonlit Forest
    -- gains 'Take 1 damage.'" Modeled as a forced reaction on revealing a moon token while
    -- at Dead Grove or an adjacent Moonlit Forest.
    extendRevealed1 a
      $ restricted
        a
        1
        (youExist $ at_ (oneOf [be a, LocationWithTitle "Moonlit Forest" <> connectedTo (be a)]))
      $ forced
      $ RevealChaosToken #after You (ChaosTokenFaceIs MoonToken)

instance RunMessage MoonlitForestDeadGrove where
  runMessage msg l@(MoonlitForestDeadGrove attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid (attrs.ability 1) 1
      pure l
    _ -> MoonlitForestDeadGrove <$> liftRunMessage msg attrs
