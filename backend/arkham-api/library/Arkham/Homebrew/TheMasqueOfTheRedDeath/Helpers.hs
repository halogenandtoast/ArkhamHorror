{- | Shared rules for The Masque of the Red Death.

The masquerade is a toll road. Six of the seven coloured chambers charge the
same additional cost to enter -- clues as a group, or doom on the [[Guest]]
asset standing in that room -- so every chamber the party opens either drains
the clue pool act 2 needs or winds the agenda forward. The Black Chamber charges
clues only.

Each chamber also prints its own @[skull]@ effect on its revealed face, and the
agendas progressively staple those effects onto more and more chaos tokens. That
is the scenario's difficulty curve, and it is why so many cards here count
"@[skull]@ effects on your location" rather than modifiers.
-}
module Arkham.Homebrew.TheMasqueOfTheRedDeath.Helpers where

import Arkham.Ability
import Arkham.Asset.Types (AssetAttrs)
import Arkham.Card.CardCode (HasCardCode)
import Arkham.Card.CardDef (CardDef)
import Arkham.ChaosToken (pattern NegativeModifier, pattern PositiveModifier)
import Arkham.ChaosToken.Types (ChaosToken, ChaosTokenModifier (NoModifier))
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue (push, pushAll)
import Arkham.Classes.Query
import Arkham.GameValue (GameValue (PerPlayer))
import Arkham.Helpers.Investigator (getCanDiscoverClues)
import Arkham.Helpers.Location (getLocationOf, withLocationOf)
import Arkham.Helpers.Modifiers (
  ModifierType (CannotMove, IgnoreChaosToken, IgnoreChaosTokenEffects),
  getModifiers,
 )
import Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Locations qualified as Locations
import Arkham.I18n
import Arkham.Id
import Arkham.Location.Types (LocationAttrs)
import Arkham.Matcher hiding (IgnoreChaosToken)
import Arkham.Message (
  Message (AdvanceAgendaIfThresholdSatisfied, Do, UseCardAbility),
  pattern PlaceDoom,
 )
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Choose (chooseOneM, labeled)
import Arkham.Prelude
import Arkham.Source (Source (PaymentSource), Sourceable)
import Arkham.Target (Target (AssetTarget), toTarget)
import Arkham.Trait (Trait (Guest))
import Arkham.Window (defaultWindows)

{- | The eight masked guests, in collector order.

Setup removes two at random and deals the other six to the chambers, so nothing
may assume a particular guest is in the game.
-}
guests :: [CardDef]
guests =
  [ Assets.theDevilsMorbidlyFascinated
  , Assets.theFoxLookingForAction
  , Assets.theMothDrawnToTheFlame
  , Assets.theOwlReadingIntoYou
  , Assets.thePeacockCenterOfAttention
  , Assets.theRavenNotEasilyImpressed
  , Assets.theVultureWatchingTheFeast
  , Assets.theWaspViolentlyMotivated
  ]

{- | The six chambers that get a guest, paired with their grid label -- every
[[Manor]] location except Grand Ballroom and Black Chamber.
-}
guestChambers :: [(Text, CardDef)]
guestChambers =
  [ ("blueChamber", Locations.blueChamber)
  , ("purpleChamber", Locations.purpleChamber)
  , ("greenChamber", Locations.greenChamber)
  , ("orangeChamber", Locations.orangeChamber)
  , ("whiteChamber", Locations.whiteChamber)
  , ("violetChamber", Locations.violetChamber)
  ]

{- | The marker window carried by every @[skull]@ effect.

It never matches a real window on purpose: a @[skull]@ effect is a line of text
on a location, not a reaction, and it resolves only when a token resolution
reaches for it ('addSkullEffectsToToken'). The value exists so
'skullEffectsOn' can select on it, and nothing else in the engine writes it.
-}
skullEffectWindow :: WindowMatcher
skullEffectWindow = OrWindowMatcher [NotAnyWindow]

{- | A @[skull]@ effect: either the one a chamber prints on its revealed face, or
one a card grants a location ("your location gains: @[skull]@: ...").

A granted one is proxied onto the location it is granted to, which may be named
by a matcher -- @skullEffect (proxied (locationWithInvestigator iid) a) 1@
resolves to whichever location that is, and the card handles
@UseThisAbility iid (isProxySource attrs -> True) 1@.

Every one of them declares itself on the @[skull]@ face, so the skill test
window lists it -- attributed to this card -- before any token is revealed. The
declaration carries no value because this seam does not know one; prefer
'describedSkullEffect', which supplies the value and the prose the window shows.
-}
skullEffect :: (HasCardCode a, Sourceable a) => a -> Int -> Ability
skullEffect a n =
  affectsChaosToken #skull NoModifier $ mkAbility a n $ SilentForcedAbility skullEffectWindow

{- | 'skullEffect' that also tells the skill test window what it does, so the
effect reads as the card prints it instead of as a bare card name.

@value@ is what the effect adds to a test when the token is revealed, which the
window folds into the face's total ahead of the draw. Pass @0@ when the card
prints no number, or when the number is only known at resolution time -- Grand
Ballroom's "-1 for every 2 revealed Manor locations" is computed, so it declares
0 and carries the whole clause in its text instead of showing a wrong prediction.

@key@ names the scenario-scoped locale entry holding the printed effect minus any
leading value, since the window renders the value itself; the empty key is for a
card whose effect is only a number. The entry takes the usual card markup:
@{skull}@ for an icon, @_Manor_@ for a trait.
-}
describedSkullEffect :: (HasCardCode a, Sourceable a) => Int -> Text -> a -> Int -> Ability
describedSkullEffect value key a n =
  (if null key then id else scenarioI18n (withI18nTooltip key))
    $ affectsChaosToken #skull modifier
    $ skullEffect a n
 where
  modifier
    | value > 0 = PositiveModifier value
    | value < 0 = NegativeModifier (abs value)
    | otherwise = NoModifier

-- | Every @[skull]@ effect on a location, printed or granted.
skullEffectsOn :: LocationId -> AbilityMatcher
skullEffectsOn lid = AbilityOnLocation (LocationWithId lid) <> AbilityWindow skullEffectWindow

-- | "the number of @[skull]@ effects on its location"
countSkullEffectsOn :: HasGame m => LocationId -> m Int
countSkullEffectsOn = selectCount . skullEffectsOn

{- | "Add each @[skull]@ effect on your location to this token."

Each effect resolves for @iid@ -- the investigator who revealed the token -- so
its own numeric part and its rider both land on that investigator's test. A
token whose effects are being ignored adds nothing, matching the gate
'Arkham.Scenario' already puts in front of a scenario's own token effects.
-}
addSkullEffectsToToken :: ReverseQueue m => InvestigatorId -> ChaosToken -> m ()
addSkullEffectsToToken iid token = do
  mods <- foldMapM getModifiers [toTarget token.face, toTarget token]
  unless (any (`elem` mods) [IgnoreChaosTokenEffects, IgnoreChaosToken]) do
    withLocationOf iid \lid -> do
      effects <- select $ skullEffectsOn lid
      pushAll [UseCardAbility iid ab.source ab.index (defaultWindows iid) NoPayment | ab <- effects]

{- | The additional cost to enter a coloured chamber: @1[per_investigator]@ clues
as a group, or doom on the [[Guest]] standing in that room.

"As a group" names no group, so every investigator may contribute the clues
(Grimoire, @glossary/costs@: "each investigator ... may contribute"), hence
'Anywhere' rather than the entering investigator's location. The doom branch is
offered only at the player count that prints it, and only while a [[Guest]] is
still there to take it -- 'AssetDoomCost' is unaffordable with nothing to match,
which is what drops that half of the 'OrCost'.

The doom branch is labeled, since 'AssetDoomCost' renders as "place N doom on a card
you control" -- true of the cost that named the type, wrong for a toll paid by the
masked guest standing in the room.
-}
chamberToll :: Cost
chamberToll =
  OrCost
    [ GroupClueCost (PerPlayer 1) Anywhere
    , CostOnlyWhen onePlayerOrTwo (doomOnGuest 1)
    , CostOnlyWhen (not_ onePlayerOrTwo) (doomOnGuest 2)
    ]
 where
  onePlayerOrTwo = oneOf [PlayerCountIs 1, PlayerCountIs 2]
  guestHere = AssetWithTrait Guest <> AssetAt ThisLocation
  doomOnGuest n =
    LabeledCost
      (campaignI18n $ scope "chamberToll" $ countVar n $ "$" <> labelKey "doomOnGuest")
      (AssetDoomCost n guestHere)

{- | "This doom can cause the current agenda to advance."

Doom placed outside the mythos phase advances nothing on its own, so a chamber
whose toll was paid in doom has to ask for the check. Call it before delegating
to the base runner.

The hook is the payment itself rather than the move that charged it: the toll is
not part of the move ability's cost. 'CostToEnterUnrevealed' is a placeholder the
ability never pays ("unwrapped elsewhere"), and movement re-raises it as its own
'PayAdditionalCost', whose active cost reports neither the ability nor what was
spent. So match doom landing on this chamber's [[Guest]] as a cost payment. A
[[Victim]]'s own entry toll is an ability rather than a cost, and prints no
advance clause, so it stays out.

The committed @Do@ form, because the plain 'PlaceTokens' only opens the when-window
-- the asset defers the counter itself to the @Do@, so a check asked for any earlier
counts the toll's doom as not yet paid.
-}
tollDoomMayAdvanceAgenda :: ReverseQueue m => LocationAttrs -> Message -> m ()
tollDoomMayAdvanceAgenda attrs = \case
  Do (PlaceDoom (PaymentSource _) (AssetTarget aid) n) | n > 0 -> do
    paidHere <- aid <=~> (AssetWithTrait Guest <> AssetAt (LocationWithId attrs.id))
    when paidHere $ push AdvanceAgendaIfThresholdSatisfied
  _ -> pure ()

-- | The campaign's own i18n scope; the folder name camelCased.
campaignI18n :: (HasI18n => a) -> a
campaignI18n a = withI18n $ scope "theMasqueOfTheRedDeath" a

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "scenario" a

{- | "If you succeed, either remove a doom from \<guest\> or discover a clue at
your location."

The rider on every parley the sixteen guest faces print; @n@ is the parley's own
ability index, 1 on a [[Guest]] face and 2 on a [[Victim]] face. Each half is
offered only when it can do something -- doom to remove, or a clue this
investigator is actually allowed to discover here -- and with neither available
there is nothing to ask.
-}
guestParleySuccess :: ReverseQueue m => AssetAttrs -> Int -> InvestigatorId -> m ()
guestParleySuccess attrs n iid = do
  canDiscover <- maybe (pure False) (getCanDiscoverClues NotInvestigate iid) =<< getLocationOf iid
  when (attrs.doom > 0 || canDiscover) do
    chooseOneM iid $ withI18n do
      when (attrs.doom > 0) do
        countVar 1 $ labeled "removeDoom" $ removeDoom (attrs.ability n) attrs 1
      when canDiscover do
        countVar 1
          $ labeled "discoverAtYourLocation"
          $ discoverAtYourLocation NotInvestigate iid (attrs.ability n) 1

{- | "You must either place 1 doom on \<guest\>, or you cannot leave \<guest\>'s
location until the end of the round."

The @Forced@ entry toll on every [[Victim]] face, always ability 1. The second
half is a movement restriction rather than a cost, so both halves are offered
whatever the state. None of the eight prints "this may cause the current agenda
to advance", so unlike the chamber toll this doom gets no advance check.
-}
guestEntryToll :: ReverseQueue m => AssetAttrs -> InvestigatorId -> m ()
guestEntryToll attrs iid =
  chooseOneM iid $ scenarioI18n $ nameVar attrs do
    unscoped $ countVar 1 $ labeled "placeDoomOn" $ placeDoom (attrs.ability 1) attrs 1
    scope "guest" $ labeled "cannotLeave" $ roundModifier (attrs.ability 1) iid CannotMove
