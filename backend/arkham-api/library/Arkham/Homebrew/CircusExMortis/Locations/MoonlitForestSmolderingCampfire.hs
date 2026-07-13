module Arkham.Homebrew.CircusExMortis.Locations.MoonlitForestSmolderingCampfire (
  moonlitForestSmolderingCampfire,
) where

import Arkham.Ability
import Arkham.Card (toCard, card_)
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Token (Token (..))
import Arkham.Trait (Trait (Woods))

newtype MoonlitForestSmolderingCampfire
  = MoonlitForestSmolderingCampfire LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moonlitForestSmolderingCampfire
  :: LocationCard MoonlitForestSmolderingCampfire
moonlitForestSmolderingCampfire =
  locationWith
    MoonlitForestSmolderingCampfire
    Cards.moonlitForestSmolderingCampfire
    1
    (Static 1)
    connectsToAdjacent

instance HasAbilities MoonlitForestSmolderingCampfire where
  getAbilities (MoonlitForestSmolderingCampfire a) =
    extendRevealed
      a
      [ mkAbility a 1 $ forced $ RevealLocation #after You (be a)
      , -- TODO(homebrew): the printed "additional cost to move from this location to any
        -- other [[Woods]] location" is modeled as an after-move forced effect, not a
        -- pre-move payment.
        mkAbility a 2 $ forced $ Moves #after You AnySource (be a) (LocationWithTrait Woods)
      ]

instance RunMessage MoonlitForestSmolderingCampfire where
  runMessage msg l@(MoonlitForestSmolderingCampfire attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectOne (locationIs Cards.remoteCabin)
        >>= traverse_ \lid -> placeTokens (attrs.ability 1) lid Damage 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      findEncounterCard iid attrs $ card_ #enemy
      pure l
    FoundEncounterCard iid (isTarget attrs -> True) card -> do
      withLocationOf iid \lid -> push $ SpawnEnemyAtEngagedWith (toCard card) lid iid
      pure l
    _ -> MoonlitForestSmolderingCampfire <$> liftRunMessage msg attrs
