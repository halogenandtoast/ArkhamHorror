module Arkham.Homebrew.CircusExMortis.Locations.MoonlitForestQuietValley (
  moonlitForestQuietValley,
) where

import Arkham.Ability
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Token (Token (..))
import Arkham.Trait (Trait (Hex, Woods))

newtype MoonlitForestQuietValley = MoonlitForestQuietValley LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moonlitForestQuietValley :: LocationCard MoonlitForestQuietValley
moonlitForestQuietValley =
  locationWith
    MoonlitForestQuietValley
    Cards.moonlitForestQuietValley
    2
    (Static 1)
    connectsToAdjacent

instance HasAbilities MoonlitForestQuietValley where
  getAbilities (MoonlitForestQuietValley a) =
    extendRevealed
      a
      [ mkAbility a 1 $ forced $ RevealLocation #after You (be a)
      , -- TODO(homebrew): printed as an additional cost to move; modeled as an after-move
        -- forced effect rather than a pre-move payment.
        mkAbility a 2 $ forced $ Moves #after You AnySource (be a) (LocationWithTrait Woods)
      ]

instance RunMessage MoonlitForestQuietValley where
  runMessage msg l@(MoonlitForestQuietValley attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectOne (locationIs Cards.remoteCabin)
        >>= traverse_ \lid -> placeTokens (attrs.ability 1) lid Damage 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      findEncounterCard iid attrs (#treachery <> CardWithTrait Hex)
      pure l
    FoundEncounterCard iid (isTarget attrs -> True) card -> do
      drawCard iid card
      pure l
    _ -> MoonlitForestQuietValley <$> liftRunMessage msg attrs
