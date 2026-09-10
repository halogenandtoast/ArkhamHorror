module Arkham.Homebrew.CircusExMortis.Locations.MoonlitForestDeadGrove (
  moonlitForestDeadGrove,
) where

import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.CircusExMortis.Tokens (pattern MoonToken)
import Arkham.Location.Import.Lifted hiding (RevealChaosToken)
import Arkham.Matcher

newtype MoonlitForestDeadGrove = MoonlitForestDeadGrove LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

moonlitForestDeadGrove :: LocationCard MoonlitForestDeadGrove
moonlitForestDeadGrove =
  location
    MoonlitForestDeadGrove
    Cards.moonlitForestDeadGrove
    3
    (Static 2)

instance RunMessage MoonlitForestDeadGrove where
  runMessage msg l@(MoonlitForestDeadGrove attrs) = runQueueT $ case msg of
    -- "Each moon token revealed at this location and each adjacent copy of Moonlit Forest
    -- gains 'Take 1 damage.'" This rides the token's own resolution rather than a forced
    -- reaction so that every moon token in a test deals its damage.
    ResolveChaosToken _ MoonToken iid | attrs.revealed -> do
      here <-
        iid
          <=~> InvestigatorAt (oneOf [be attrs, LocationWithTitle "Moonlit Forest" <> connectedTo (be attrs)])
      when here $ assignDamage iid attrs 1
      pure l
    _ -> MoonlitForestDeadGrove <$> liftRunMessage msg attrs
