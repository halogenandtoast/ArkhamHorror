module Arkham.Homebrew.CircusExMortis.Locations.MoonlitForestShallowRiver (
  moonlitForestShallowRiver,
) where

import Arkham.Ability
import Arkham.Helpers.Message.Discard.Lifted (chooseAndDiscardCard)
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Token (Token (..))
import Arkham.Trait (Trait (Woods))

newtype MoonlitForestShallowRiver = MoonlitForestShallowRiver LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moonlitForestShallowRiver :: LocationCard MoonlitForestShallowRiver
moonlitForestShallowRiver =
  locationWith
    MoonlitForestShallowRiver
    Cards.moonlitForestShallowRiver
    2
    (Static 1)
    connectsToAdjacent

instance HasAbilities MoonlitForestShallowRiver where
  getAbilities (MoonlitForestShallowRiver a) =
    extendRevealed
      a
      [ mkAbility a 1 $ forced $ RevealLocation #after You (be a)
      , -- TODO(homebrew): printed as an additional cost to move; modeled as an after-move
        -- forced effect rather than a pre-move payment.
        mkAbility a 2 $ forced $ Moves #after You AnySource (be a) (LocationWithTrait Woods)
      ]

instance RunMessage MoonlitForestShallowRiver where
  runMessage msg l@(MoonlitForestShallowRiver attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectOne (locationIs Cards.woodlandOverlook)
        >>= traverse_ \lid -> placeTokens (attrs.ability 1) lid Damage 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      chooseAndDiscardCard iid (attrs.ability 2)
      loseResources iid (attrs.ability 2) 1
      pure l
    _ -> MoonlitForestShallowRiver <$> liftRunMessage msg attrs
