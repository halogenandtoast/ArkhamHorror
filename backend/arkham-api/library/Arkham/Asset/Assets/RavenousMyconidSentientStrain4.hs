module Arkham.Asset.Assets.RavenousMyconidSentientStrain4 (ravenousMyconidSentientStrain4) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Event.Cards qualified as Events
import Arkham.Helpers.Investigator (searchBondedJust)
import Arkham.Helpers.Window (cardDrawn)
import Arkham.Matcher
import Arkham.Taboo
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
        $ controlled a 1 (youExist $ InvestigatorWithBondedCard $ cardIs Events.uncannyGrowth)
        $ FastAbility Free
    , controlled_ a 2
        $ freeReaction
        $ oneOf
        $ [((You <>), id), (affectsOthers . (not_ You <>), (NonPeril <>))]
        & map \(f, g) ->
          DrawCard
            #when
            (f $ at_ (LocationWithShroud $ (if tabooed TabooList25 a then lessThan else atMost) $ a.use Growth))
            (CanCancelRevelationEffect You $ basic $ g NonWeaknessTreachery)
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
