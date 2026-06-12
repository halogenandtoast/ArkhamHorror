module Arkham.Investigator.Cards.FatherMateoParallel (fatherMateoParallel) where

import Arkham.Ability
import Arkham.ChaosBagStepState
import Arkham.ChaosToken
import Arkham.Helpers.Window hiding (checkAfter)
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher hiding (DuringTurn, RevealChaosToken)
import Arkham.Message.Lifted.Choose
import Arkham.Window qualified as Window

newtype FatherMateoParallel = FatherMateoParallel InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

fatherMateoParallel :: InvestigatorCard FatherMateoParallel
fatherMateoParallel =
  investigator FatherMateoParallel Cards.fatherMateoParallel
    $ Stats {health = 6, sanity = 8, willpower = 4, intellect = 3, combat = 2, agility = 3}

instance HasAbilities FatherMateoParallel where
  getAbilities (FatherMateoParallel a) =
    [ playerLimit PerRound
        $ selfAbility
          a
          1
          ( DuringTurn You
              <> exists (affectsOthers $ at_ YourLocation <> not_ (InvestigatorWithSealedChaosToken #bless))
              <> ChaosTokenCountIs #bless (atLeast 1)
          )
          (FastAbility Free)
    , selfAbility_ a 2
        $ freeReaction
        $ WouldRevealChaosToken #when
        $ InvestigatorWithSealedChaosToken (#bless <> not_ (mapOneOf chaosTokenIs $ pendingTokens a))
    ]

pendingTokens :: InvestigatorAttrs -> [ChaosToken]
pendingTokens = lookupMetaKeyWithDefault "father_mateo_pending" []

instance HasChaosTokenValue FatherMateoParallel where
  getChaosTokenValue iid ElderSign (FatherMateoParallel attrs) | iid == attrs.id = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 1)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage FatherMateoParallel where
  runMessage msg i@(FatherMateoParallel attrs) = runQueueT $ case msg of
    -- The "father_mateo_pending" guard only needs to prevent ability 2 from
    -- offering the same sealed bless twice within a single reveal sequence. It
    -- is normally cleared when the revealed token is revealed/resolved, but
    -- those messages are not guaranteed to fire (e.g. an interrupted reveal),
    -- which can leak an entry into the meta and permanently disable the
    -- reaction (issue #4774). The start of a skill test is a safe reset point:
    -- nothing is mid-reveal yet, so clearing it here cannot drop a live token
    -- and self-heals any stale state.
    BeginSkillTestWithPreMessages' {} -> do
      attrs' <- liftRunMessage msg attrs
      pure . FatherMateoParallel $ attrs' & setMetaKey "father_mateo_pending" ([] :: [ChaosToken])
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      do_ msg
      pure i
    Do (UseThisAbility iid (isSource attrs -> True) 1) -> do
      investigators <-
        select
          $ affectsOthers
          $ at_ (locationWithInvestigator iid)
          <> not_ (InvestigatorWithSealedChaosToken #bless)
      chooseOrRunTargetM iid investigators $ handleTarget iid (attrs.ability 1)
      pure i
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (InvestigatorTarget iid') -> do
      tokens <- select $ ChaosTokenFaceIs #bless
      focusChaosTokens tokens \unfocus -> do
        chooseTargetM iid tokens \t -> do
          push $ SealChaosToken t
          push $ SealedChaosToken t (Just iid) (toTarget iid')
          push unfocus
      pure i
    UseCardAbility iid (isSource attrs -> True) 2 (wouldRevealChaosToken -> iid') _ -> do
      selectOne (SealedOnInvestigator (InvestigatorWithId iid') #bless) >>= \case
        Nothing -> pure i
        Just token -> do
          push $ SilentRevealChaosToken (attrs.ability 2) iid' token
          checkAfter $ Window.RevealChaosToken iid token
          push
            $ ReplaceCurrentDraw (attrs.ability 2) iid
            $ Choose (attrs.ability 2) 1 ResolveChoice [Resolved [token]] [] Nothing
          pure . FatherMateoParallel $ attrs & setMetaKey "father_mateo_pending" (token : pendingTokens attrs)
    RevealChaosToken _ iid' drawnToken | drawnToken.face == #bless -> do
      whenM (drawnToken <=~> SealedOnInvestigator (InvestigatorWithId iid') #bless) do
        push $ UnsealChaosToken drawnToken

      pure
        . FatherMateoParallel
        $ attrs
        & setMetaKey "father_mateo_pending" (filter (/= drawnToken) $ pendingTokens attrs)
    ResolveChaosToken drawnToken face iid' | face == #bless -> do
      whenM (drawnToken <=~> SealedOnInvestigator (InvestigatorWithId iid') #bless) do
        push $ UnsealChaosToken drawnToken

      pure
        . FatherMateoParallel
        $ attrs
        & setMetaKey "father_mateo_pending" (filter (/= drawnToken) $ pendingTokens attrs)
    ElderSignEffect (is attrs -> True) -> do
      hasBless <- selectAny $ ChaosTokenFaceIs #bless
      if hasBless
        then liftRunMessage (Do $ UseCardAbility attrs.id (toSource attrs) 1 [] NoPayment) i
        else pure i
    _ -> FatherMateoParallel <$> liftRunMessage msg attrs
