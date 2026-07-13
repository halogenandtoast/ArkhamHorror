module Arkham.Location.Cards.MoonlitForestShallowRiverCircusExMortis (
  moonlitForestShallowRiverCircusExMortis,
) where

import Arkham.Ability
import Arkham.Helpers.Message.Discard.Lifted (chooseAndDiscardCard)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Token (Token (..))
import Arkham.Trait (Trait (Woods))

newtype MoonlitForestShallowRiverCircusExMortis = MoonlitForestShallowRiverCircusExMortis LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moonlitForestShallowRiverCircusExMortis :: LocationCard MoonlitForestShallowRiverCircusExMortis
moonlitForestShallowRiverCircusExMortis =
  locationWith
    MoonlitForestShallowRiverCircusExMortis
    Cards.moonlitForestShallowRiverCircusExMortis
    2
    (Static 1)
    connectsToAdjacent

instance HasAbilities MoonlitForestShallowRiverCircusExMortis where
  getAbilities (MoonlitForestShallowRiverCircusExMortis a) =
    extendRevealed
      a
      [ mkAbility a 1 $ forced $ RevealLocation #after You (be a)
      , -- TODO(homebrew): printed as an additional cost to move; modeled as an after-move
        -- forced effect rather than a pre-move payment.
        mkAbility a 2 $ forced $ Moves #after You AnySource (be a) (LocationWithTrait Woods)
      ]

instance RunMessage MoonlitForestShallowRiverCircusExMortis where
  runMessage msg l@(MoonlitForestShallowRiverCircusExMortis attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectOne (locationIs Cards.woodlandOverlookCircusExMortis)
        >>= traverse_ \lid -> placeTokens (attrs.ability 1) lid Damage 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      chooseAndDiscardCard iid (attrs.ability 2)
      loseResources iid (attrs.ability 2) 1
      pure l
    _ -> MoonlitForestShallowRiverCircusExMortis <$> liftRunMessage msg attrs
