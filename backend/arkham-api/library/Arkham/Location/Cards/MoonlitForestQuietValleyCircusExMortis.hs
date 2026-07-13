module Arkham.Location.Cards.MoonlitForestQuietValleyCircusExMortis (
  moonlitForestQuietValleyCircusExMortis,
) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Token (Token (..))
import Arkham.Trait (Trait (Hex, Woods))

newtype MoonlitForestQuietValleyCircusExMortis = MoonlitForestQuietValleyCircusExMortis LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moonlitForestQuietValleyCircusExMortis :: LocationCard MoonlitForestQuietValleyCircusExMortis
moonlitForestQuietValleyCircusExMortis =
  locationWith
    MoonlitForestQuietValleyCircusExMortis
    Cards.moonlitForestQuietValleyCircusExMortis
    2
    (Static 1)
    connectsToAdjacent

instance HasAbilities MoonlitForestQuietValleyCircusExMortis where
  getAbilities (MoonlitForestQuietValleyCircusExMortis a) =
    extendRevealed
      a
      [ mkAbility a 1 $ forced $ RevealLocation #after You (be a)
      , -- TODO(homebrew): printed as an additional cost to move; modeled as an after-move
        -- forced effect rather than a pre-move payment.
        mkAbility a 2 $ forced $ Moves #after You AnySource (be a) (LocationWithTrait Woods)
      ]

instance RunMessage MoonlitForestQuietValleyCircusExMortis where
  runMessage msg l@(MoonlitForestQuietValleyCircusExMortis attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectOne (locationIs Cards.remoteCabinCircusExMortis)
        >>= traverse_ \lid -> placeTokens (attrs.ability 1) lid Damage 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      findEncounterCard iid attrs (#treachery <> CardWithTrait Hex)
      pure l
    FoundEncounterCard iid (isTarget attrs -> True) card -> do
      drawCard iid card
      pure l
    _ -> MoonlitForestQuietValleyCircusExMortis <$> liftRunMessage msg attrs
