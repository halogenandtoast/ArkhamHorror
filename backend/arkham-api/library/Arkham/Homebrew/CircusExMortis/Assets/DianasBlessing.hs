module Arkham.Homebrew.CircusExMortis.Assets.DianasBlessing (dianasBlessing) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.ChaosToken (getModifiedChaosTokenFaces)
import Arkham.Homebrew.CircusExMortis.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.CircusExMortis.CardDefs.Stories qualified as Stories
import Arkham.Homebrew.CircusExMortis.Helpers (
  flipToVictoryDisplay,
  investigatorWithDestinyModifier,
  moonToken,
 )
import Arkham.Matcher

{- | The back of the Recite the Prayer Destiny story (:207b), in play next to the act deck.

"The investigator whose destiny is \"prayer\" may activate abilities on this card at any
location." The card is at no location at all, so nothing could otherwise be reached here;
the sentence names one seat, and that seat is the whole criterion on both abilities -- no
location requirement, which is what "at any location" means.

"The [elder_sign] token cannot be sealed." NOT IMPLEMENTED: the engine has no
seal-permission seam (no modifier, criterion or matcher is consulted when a card seals a
token), so honouring it would need a new modifier read at every seal site. It is inert
against everything in this scenario and against the player cards that seal, all of which
already exclude [elder_sign] from their own choices.
-}
newtype DianasBlessing = DianasBlessing AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dianasBlessing :: AssetCard DianasBlessing
dianasBlessing = asset DianasBlessing Cards.dianasBlessing

{- | 1. "[reaction] When a ☾ token is released from a card at your location, place 1
resource on Diana's Blessing." 'ChaosTokenReleased' names the card the token was sealed on
and substitutes @You@ per candidate seat, so @InvestigatorAt YourLocation@ reads "released
from the investigator card of anyone at your location" -- which is where every ☾ token in
this scenario is sealed.
2. "[action]: Reveal X random chaos tokens from the chaos bag, where X is the number of
resources on Diana's Blessing." X of 0 reveals nothing, so the action is not offered until
there is a resource to count. ('ResourcesOnThis', not 'TokensOnThis': the latter only
implements a treachery source and errors on an asset one.)
-}
instance HasAbilities DianasBlessing where
  getAbilities (DianasBlessing a) =
    [ restricted a 1 prayerSeat
        $ freeReaction (ChaosTokenReleased #when (InvestigatorAt YourLocation) moonToken)
    , restricted a 2 (prayerSeat <> ResourcesOnThis (atLeast 1)) actionAbility
    ]
   where
    prayerSeat = youExist (investigatorWithDestinyModifier "prayer")

instance RunMessage DianasBlessing where
  runMessage msg a@(DianasBlessing attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeTokens (attrs.ability 1) attrs #resource 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      requestChaosTokens iid (attrs.ability 2) (attrs.token #resource)
      pure a
    -- "If an [elder_sign] token is revealed, flip Diana's Blessing and add it to the
    -- victory display." Read through the modifier layer, so a token being treated as an
    -- [elder_sign] counts, the same way every other "if X is revealed" rider does.
    RequestedChaosTokens (isAbilitySource attrs 2 -> True) _ tokens -> do
      faces <- getModifiedChaosTokenFaces tokens
      when (#eldersign `elem` faces)
        $ flipToVictoryDisplay Nothing Stories.reciteThePrayer attrs.cardId (RemoveAsset attrs.id)
      pure a
    _ -> DianasBlessing <$> liftRunMessage msg attrs
