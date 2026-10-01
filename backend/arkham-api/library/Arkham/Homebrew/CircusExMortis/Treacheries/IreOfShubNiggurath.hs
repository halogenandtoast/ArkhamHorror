module Arkham.Homebrew.CircusExMortis.Treacheries.IreOfShubNiggurath (ireOfShubNiggurath) where

import Arkham.Ability
import Arkham.ChaosToken
import Arkham.Classes.HasQueue (popMessageMatching_)
import Arkham.Helpers.ChaosToken (getModifiedChaosTokenFaces)
import Arkham.Homebrew.CircusExMortis.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (moonToken)
import Arkham.Homebrew.CircusExMortis.Tokens (pattern MoonToken)
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Treachery.Import.Lifted
import Arkham.Window (Window (windowType))
import Arkham.Window qualified as Window

newtype IreOfShubNiggurath = IreOfShubNiggurath TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ireOfShubNiggurath :: TreacheryCard IreOfShubNiggurath
ireOfShubNiggurath = treachery IreOfShubNiggurath Cards.ireOfShubNiggurath

{- | One activation, carried in enough detail to find its queued 'UseCardAbility'
exactly. `ActiveCost`'s `PayCostFinished` queues
@UseCardAbility iid ability.source ability.index c.windows c.payments@ and opens the
@ActivateAbility #when@ window on the same @c.windows@, so all four of these come
straight off the window payload and identify that one message. Only the payment is
unknown here, and nothing else in the queue can share the other four.
-}
data Activation = Activation
  { activationInvestigator :: InvestigatorId
  , activationSource :: Source
  , activationIndex :: Int
  , activationWindows :: [Window]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Everything that has to match for a queued 'UseCardAbility' to BE this activation.
activationKey :: Activation -> (InvestigatorId, Source, Int, [Window])
activationKey a =
  (a.activationInvestigator, a.activationSource, a.activationIndex, a.activationWindows)

{- | @pending@ bridges the chaos-token round trip: the reveal answers back as
'RequestedChaosTokens', whose only payload is a source, so the activation being
interrupted has to be parked somewhere. @metaTriggered@ is the "(Max once per ability
each round.)" ledger, keyed by the activated ability rather than by this card.
-}
data Meta = Meta
  { metaPending :: Maybe Activation
  , metaTriggered :: [(Source, Int)]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

ireMeta :: TreacheryAttrs -> Meta
ireMeta a = toResultDefault (Meta Nothing []) a.meta

doomFaces :: [ChaosTokenFace]
doomFaces = [Skull, Cultist, Tablet, ElderThing, AutoFail, MoonToken]

-- | The activation the @ActivateAbility #when@ window Ire just triggered off describes.
activatedAbility :: [Window] -> Maybe Activation
activatedAbility ws =
  listToMaybe
    [ Activation iid ability.source ability.index tws
    | Window.ActivateAbility iid tws ability <- map windowType ws
    ]

instance HasAbilities IreOfShubNiggurath where
  getAbilities (IreOfShubNiggurath a) =
    [ restricted a 1 (InThreatAreaOf You) $ forced $ ActivateAbility #when You (interrupted bearer)
    | bearer <- toList a.inThreatAreaOf
    ]
      -- "released at your location": every ☾ in this scenario is sealed on an
      -- investigator card, so the release is matched on its bearer rather than
      -- through `ChaosTokenReleasedFrom`.
      <> [ restricted a 2 (InThreatAreaOf You)
             $ freeReaction
             $ ChaosTokenReleased #after (InvestigatorAt YourLocation) moonToken
         ]
   where
    alreadyInterrupted = (ireMeta a).metaTriggered
    {- The round's ledger is spent through the window matcher, not checked after the
    fact: an ability already interrupted this round must not re-open this Forced at
    all, since a second trigger could change nothing. -}
    interrupted bearer =
      mconcat
        $ [oneOf [AbilityIsFastAbility, AbilityIsReactionAbility], AbilityOnCardControlledBy bearer]
        <> [ not_ (oneOf [AbilityIs s i | (s, i) <- alreadyInterrupted])
           | notNull alreadyInterrupted
           ]

instance RunMessage IreOfShubNiggurath where
  runMessage msg t@(IreOfShubNiggurath attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseCardAbility iid (isSource attrs -> True) 1 (activatedAbility -> Just activation) _ -> do
      -- A bare reveal, not a skill test draw: `ResolveChaosToken` never runs, so a
      -- revealed ☾ does not seal itself here (as on Duplicitous Illusion).
      requestChaosTokens iid (attrs.ability 1) 1
      -- Recorded on trigger, not on cancel: the max limits triggering the Forced,
      -- whichever face comes out.
      let m = ireMeta attrs
      pure
        $ IreOfShubNiggurath
        $ attrs
        & setMeta
          m
            { metaPending = Just activation
            , metaTriggered = (activation.activationSource, activation.activationIndex) : m.metaTriggered
            }
    RequestedChaosTokens (isAbilitySource attrs 1 -> True) (Just iid) tokens -> do
      faces <- getModifiedChaosTokenFaces tokens
      let m = ireMeta attrs
      continue_ iid
      when (any (`elem` doomFaces) faces) $ for_ m.metaPending \activation -> do
        {- "Cancel that activation." Costs are already paid when this window opens and
        are NOT refunded (RR "Cancel"), which is also exactly what the engine's own
        cancel does: `CancelCostPayment` (#5545) suppresses only the `UseCardAbility`
        and leaves the after-ActivateAbility window standing. Popping the queued
        activation reproduces that path message-for-message.

        A flat pop is correct here, not `popMessagesMatchingNested`: the message is
        queued by `PayCostFinished`'s own flat `pushAll`, and a `Would` batch unrolls
        one message at a time (`Would bId (x:xs) -> pushAll [x, Would bId xs]`), so
        `PayCostFinished` always runs at top level and its output lands there too.
        If that `pushAll` is ever batched or wrapped in `simultaneously`, this must
        switch to the nested variant. -}
        lift $ popMessageMatching_ \case
          UseCardAbility uIid uSource uIndex uWindows _ ->
            (uIid, uSource, uIndex, uWindows) == activationKey activation
          _ -> False
        -- The ban lives on the investigator: `preventedByInvestigatorModifiers` reads
        -- `CannotTriggerAbilityMatching` off `getModifiers (InvestigatorTarget iid)`.
        phaseModifier (attrs.ability 1) activation.activationInvestigator
          $ CannotTriggerAbilityMatching
          $ AbilityIs activation.activationSource activation.activationIndex
      pure $ IreOfShubNiggurath $ attrs & setMeta m {metaPending = Nothing}
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    EndRound -> pure $ IreOfShubNiggurath $ attrs & setMeta (ireMeta attrs) {metaTriggered = []}
    _ -> IreOfShubNiggurath <$> liftRunMessage msg attrs
