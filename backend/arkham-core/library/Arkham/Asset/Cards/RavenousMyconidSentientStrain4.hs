module Arkham.Asset.Cards.RavenousMyconidSentientStrain4 (
  ravenousMyconidSentientStrain4,
  RavenousMyconidSentientStrain4 (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Event.Cards qualified as Events
import Arkham.Helpers.Investigator (searchBondedJust)
import Arkham.Helpers.Window (cardDrawn)
import Arkham.Matcher
import Arkham.Token

newtype RavenousMyconidSentientStrain4 = RavenousMyconidSentientStrain4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ravenousMyconidSentientStrain4 :: AssetCard RavenousMyconidSentientStrain4
ravenousMyconidSentientStrain4 =
  assetWith
    RavenousMyconidSentientStrain4
    Cards.ravenousMyconidSentientStrain4
    ((healthL ?~ 1) . (sanityL ?~ 1))

instance HasAbilities RavenousMyconidSentientStrain4 where
  getAbilities (RavenousMyconidSentientStrain4 a) =
    [ playerLimit PerRound
        $ controlledAbility
          a
          1
          (youExist $ InvestigatorWithBondedCard $ cardIs Events.uncannyGrowth)
        $ FastAbility Free
    , restrictedAbility a 2 ControlsThis
        $ freeReaction
        $ DrawCard
          #when
          (affectsOthers $ InvestigatorAt $ LocationWithShroud (lessThan $ a.use Growth))
          (CanCancelRevelationEffect $ basic NonWeaknessTreachery)
          EncounterDeck
    ]

instance RunMessage RavenousMyconidSentientStrain4 where
  runMessage msg a@(RavenousMyconidSentientStrain4 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      uncannyGrowth <- searchBondedJust iid Events.uncannyGrowth
      addToHand iid [uncannyGrowth]
      pure a
    UseCardAbility _iid (isSource attrs -> True) 2 (cardDrawn -> card) _ -> do
      cancelCardEffects attrs card
      removeTokens (attrs.ability 2) attrs Growth (attrs.use Growth)
      pure a
    _ -> RavenousMyconidSentientStrain4 <$> liftRunMessage msg attrs
