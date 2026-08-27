module Arkham.Homebrew.CircusExMortis.Locations.MoonlitForestSmolderingCampfire (
  moonlitForestSmolderingCampfire,
) where

import Arkham.Ability
import Arkham.Card (card_, toCard)
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Token (Token (..))
import Arkham.Trait (Trait (Woods))

newtype MoonlitForestSmolderingCampfire
  = MoonlitForestSmolderingCampfire LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moonlitForestSmolderingCampfire
  :: LocationCard MoonlitForestSmolderingCampfire
moonlitForestSmolderingCampfire =
  location
    MoonlitForestSmolderingCampfire
    Cards.moonlitForestSmolderingCampfire
    1
    (Static 1)

instance HasModifiersFor MoonlitForestSmolderingCampfire where
  getModifiersFor (MoonlitForestSmolderingCampfire a) =
    modifySelect
      a
      (InvestigatorAt $ be a)
      [ AdditionalCostToEnterMatching (LocationWithTrait Woods)
          $ FindEncounterCardCost (toTarget a) [FromEncounterDeck, FromEncounterDiscard] (card_ #enemy)
      ]

instance HasAbilities MoonlitForestSmolderingCampfire where
  getAbilities (MoonlitForestSmolderingCampfire a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ RevealLocation #after You (be a)

instance RunMessage MoonlitForestSmolderingCampfire where
  runMessage msg l@(MoonlitForestSmolderingCampfire attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectOne (locationIs Cards.remoteCabin)
        >>= traverse_ \lid -> placeTokens (attrs.ability 1) lid Damage 1
      pure l
    FoundEncounterCard iid (isTarget attrs -> True) card -> do
      withLocationOf iid \lid -> push $ SpawnEnemyAtEngagedWith (toCard card) lid iid
      pure l
    _ -> MoonlitForestSmolderingCampfire <$> liftRunMessage msg attrs
