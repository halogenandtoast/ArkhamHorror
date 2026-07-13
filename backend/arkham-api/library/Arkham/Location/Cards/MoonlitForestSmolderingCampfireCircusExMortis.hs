module Arkham.Location.Cards.MoonlitForestSmolderingCampfireCircusExMortis (
  moonlitForestSmolderingCampfireCircusExMortis,
) where

import Arkham.Ability
import Arkham.Card (toCard, card_)
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Token (Token (..))
import Arkham.Trait (Trait (Woods))

newtype MoonlitForestSmolderingCampfireCircusExMortis
  = MoonlitForestSmolderingCampfireCircusExMortis LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moonlitForestSmolderingCampfireCircusExMortis
  :: LocationCard MoonlitForestSmolderingCampfireCircusExMortis
moonlitForestSmolderingCampfireCircusExMortis =
  locationWith
    MoonlitForestSmolderingCampfireCircusExMortis
    Cards.moonlitForestSmolderingCampfireCircusExMortis
    1
    (Static 1)
    connectsToAdjacent

instance HasAbilities MoonlitForestSmolderingCampfireCircusExMortis where
  getAbilities (MoonlitForestSmolderingCampfireCircusExMortis a) =
    extendRevealed
      a
      [ mkAbility a 1 $ forced $ RevealLocation #after You (be a)
      , -- TODO(homebrew): the printed "additional cost to move from this location to any
        -- other [[Woods]] location" is modeled as an after-move forced effect, not a
        -- pre-move payment.
        mkAbility a 2 $ forced $ Moves #after You AnySource (be a) (LocationWithTrait Woods)
      ]

instance RunMessage MoonlitForestSmolderingCampfireCircusExMortis where
  runMessage msg l@(MoonlitForestSmolderingCampfireCircusExMortis attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectOne (locationIs Cards.remoteCabinCircusExMortis)
        >>= traverse_ \lid -> placeTokens (attrs.ability 1) lid Damage 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      findEncounterCard iid attrs $ card_ #enemy
      pure l
    FoundEncounterCard iid (isTarget attrs -> True) card -> do
      withLocationOf iid \lid -> push $ SpawnEnemyAtEngagedWith (toCard card) lid iid
      pure l
    _ -> MoonlitForestSmolderingCampfireCircusExMortis <$> liftRunMessage msg attrs
