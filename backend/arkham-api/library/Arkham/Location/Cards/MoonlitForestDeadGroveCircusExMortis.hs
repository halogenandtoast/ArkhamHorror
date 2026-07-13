module Arkham.Location.Cards.MoonlitForestDeadGroveCircusExMortis (
  moonlitForestDeadGroveCircusExMortis,
) where

import Arkham.Ability
import Arkham.ChaosToken.Types (ChaosTokenFace (MoonToken))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted hiding (RevealChaosToken)
import Arkham.Matcher

newtype MoonlitForestDeadGroveCircusExMortis = MoonlitForestDeadGroveCircusExMortis LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moonlitForestDeadGroveCircusExMortis :: LocationCard MoonlitForestDeadGroveCircusExMortis
moonlitForestDeadGroveCircusExMortis =
  locationWith
    MoonlitForestDeadGroveCircusExMortis
    Cards.moonlitForestDeadGroveCircusExMortis
    3
    (Static 2)
    connectsToAdjacent

instance HasAbilities MoonlitForestDeadGroveCircusExMortis where
  getAbilities (MoonlitForestDeadGroveCircusExMortis a) =
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

instance RunMessage MoonlitForestDeadGroveCircusExMortis where
  runMessage msg l@(MoonlitForestDeadGroveCircusExMortis attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid (attrs.ability 1) 1
      pure l
    _ -> MoonlitForestDeadGroveCircusExMortis <$> liftRunMessage msg attrs
