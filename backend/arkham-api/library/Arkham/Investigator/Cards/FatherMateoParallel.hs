module Arkham.Investigator.Cards.FatherMateoParallel (fatherMateoParallel, FatherMateoParallel (..)) where

import Arkham.Ability
import Arkham.ChaosToken
import Arkham.ChaosBagStepState
import Arkham.Helpers.Window
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher hiding (DuringTurn, RevealChaosToken)
import Arkham.Message.Lifted.Choose

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
        $ restricted
          a
          1
          ( Self
              <> DuringTurn You
              <> exists (affectsOthers $ at_ YourLocation <> not_ (InvestigatorWithSealedChaosToken #bless))
              <> ChaosTokenCountIs #bless (atLeast 1)
          )
          (FastAbility Free)
    , restricted a 2 Self
        $ freeReaction
        $ WouldRevealChaosToken #when (InvestigatorWithSealedChaosToken $ #bless <> not_ (mapOneOf chaosTokenIs $ pendingTokens a))
    ]


pendingTokens :: InvestigatorAttrs -> [ChaosToken] 
pendingTokens = lookupMetaKeyWithDefault "father_mateo_pending" []
    

instance HasChaosTokenValue FatherMateoParallel where
  getChaosTokenValue iid ElderSign (FatherMateoParallel attrs) | iid == attrs.id = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 1)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage FatherMateoParallel where
  runMessage msg i@(FatherMateoParallel attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      push $ Do msg
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
          push $ SealedChaosToken t (toTarget iid')
          push unfocus
      pure i
    UseCardAbility iid (isSource attrs -> True) 2 (wouldRevealChaosToken -> iid') _ -> do
      selectOne (SealedOnInvestigator (InvestigatorWithId iid') #bless) >>= \case
        Nothing -> pure i
        Just token -> do
          push
            $ ReplaceCurrentDraw (attrs.ability 2) iid
            $ Choose (attrs.ability 2) 1 ResolveChoice [Resolved [token]] [] Nothing
          pure . FatherMateoParallel $ attrs & setMetaKey "father_mateo_pending" (token : pendingTokens attrs)
    RevealChaosToken _ iid' drawnToken | drawnToken.face == #bless -> do
      whenM (drawnToken <=~> SealedOnInvestigator (InvestigatorWithId iid') #bless) do
        push $ UnsealChaosToken drawnToken

      pure . FatherMateoParallel $ attrs & setMetaKey "father_mateo_pending" (filter (/= drawnToken) $ pendingTokens attrs)
    ResolveChaosToken drawnToken face iid' | face == #bless -> do
      whenM (drawnToken <=~> SealedOnInvestigator (InvestigatorWithId iid') #bless) do
        push $ UnsealChaosToken drawnToken

      pure . FatherMateoParallel $ attrs & setMetaKey "father_mateo_pending" (filter (/= drawnToken) $ pendingTokens attrs)
    ElderSignEffect (is attrs -> True) -> do
      hasBless <- selectAny $ ChaosTokenFaceIs #bless
      if hasBless
        then liftRunMessage (Do $ UseCardAbility attrs.id (toSource attrs) 1 [] NoPayment) i
        else pure i
    _ -> FatherMateoParallel <$> liftRunMessage msg attrs
