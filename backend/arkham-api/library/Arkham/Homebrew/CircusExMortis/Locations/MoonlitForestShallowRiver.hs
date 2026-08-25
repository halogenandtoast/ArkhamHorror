module Arkham.Homebrew.CircusExMortis.Locations.MoonlitForestShallowRiver (moonlitForestShallowRiver) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Token (Token (..))
import Arkham.Trait (Trait (Woods))

newtype MoonlitForestShallowRiver = MoonlitForestShallowRiver LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moonlitForestShallowRiver :: LocationCard MoonlitForestShallowRiver
moonlitForestShallowRiver =
  locationWith
    MoonlitForestShallowRiver
    Cards.moonlitForestShallowRiver
    2
    (Static 1)
    connectsToAdjacent

instance HasModifiersFor MoonlitForestShallowRiver where
  getModifiersFor (MoonlitForestShallowRiver a) =
    modifySelect
      a
      (InvestigatorAt $ be a)
      [ AdditionalCostToEnterMatching (LocationWithTrait Woods)
          $ Costs [HandDiscardCost 1 $ basic AnyCard, ResourceCost 1]
      ]

instance HasAbilities MoonlitForestShallowRiver where
  getAbilities (MoonlitForestShallowRiver a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ RevealLocation #after You (be a)

instance RunMessage MoonlitForestShallowRiver where
  runMessage msg l@(MoonlitForestShallowRiver attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectOne (locationIs Cards.woodlandOverlook)
        >>= traverse_ \lid -> placeTokens (attrs.ability 1) lid Damage 1
      pure l
    _ -> MoonlitForestShallowRiver <$> liftRunMessage msg attrs
