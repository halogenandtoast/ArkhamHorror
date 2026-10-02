module Arkham.Homebrew.CircusExMortis.Treacheries.IreOfShubNiggurath (ireOfShubNiggurath) where

import Arkham.Ability
import Arkham.ActiveCost.Base (ActiveCostTarget (..))
import Arkham.ChaosToken
import Arkham.GameEnv (getActiveCosts)
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

{- | The activation being interrupted, named by the ability it activates. That is enough
to find its 'ActiveCost' again after the chaos-token round trip.
-}
data Activation = Activation
  { activationInvestigator :: InvestigatorId
  , activationSource :: Source
  , activationIndex :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

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
    [ Activation iid ability.source ability.index
    | Window.ActivateAbility iid _ ability <- map windowType ws
    ]

-- | The 'ActiveCost' paying for exactly this activation.
isActivation :: Activation -> ActiveCostTarget -> Bool
isActivation activation = \case
  ForAbility ability ->
    ability.source == activation.activationSource && ability.index == activation.activationIndex
  _ -> False

instance HasAbilities IreOfShubNiggurath where
  getAbilities (IreOfShubNiggurath a) =
    [ restricted a 1 (InThreatAreaOf You) $ forced $ ActivateAbility #cancel You (interrupted bearer)
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
        {- "Cancel that activation." The Forced rides the @#cancel@ window, which runs
        before the ability's cost is created, so `CancelCostPayment` here stops the
        payment as well as the `UseCardAbility` -- nothing is spent on an activation
        that never happens. -}
        costs <- getActiveCosts
        for_ (find (isActivation activation . (.target)) costs) \cost ->
          push $ CancelCostPayment cost.id
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
