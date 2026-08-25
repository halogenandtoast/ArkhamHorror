module Arkham.Homebrew.CircusExMortis.Locations.MoonlitForestQuietValley (moonlitForestQuietValley) where

import Arkham.Ability
import Arkham.Card (card_)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Token (Token (..))
import Arkham.Trait (Trait (Hex, Woods))

newtype MoonlitForestQuietValley = MoonlitForestQuietValley LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moonlitForestQuietValley :: LocationCard MoonlitForestQuietValley
moonlitForestQuietValley =
  locationWith
    MoonlitForestQuietValley
    Cards.moonlitForestQuietValley
    2
    (Static 1)
    connectsToAdjacent

instance HasModifiersFor MoonlitForestQuietValley where
  getModifiersFor (MoonlitForestQuietValley a) =
    modifySelect
      a
      (InvestigatorAt $ be a)
      [ AdditionalCostToEnterMatching (LocationWithTrait Woods)
          $ FindEncounterCardCost
            (toTarget a)
            [FromEncounterDeck, FromEncounterDiscard]
            (card_ $ #treachery <> CardWithTrait Hex)
      ]

instance HasAbilities MoonlitForestQuietValley where
  getAbilities (MoonlitForestQuietValley a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ RevealLocation #after You (be a)

instance RunMessage MoonlitForestQuietValley where
  runMessage msg l@(MoonlitForestQuietValley attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectOne (locationIs Cards.remoteCabin)
        >>= traverse_ \lid -> placeTokens (attrs.ability 1) lid Damage 1
      pure l
    FoundEncounterCard iid (isTarget attrs -> True) card -> do
      drawCard iid card
      pure l
    _ -> MoonlitForestQuietValley <$> liftRunMessage msg attrs
