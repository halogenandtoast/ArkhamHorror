module Arkham.Investigator.Cards.FatherMateoParallel (fatherMateoParallel, FatherMateoParallel (..)) where

import Arkham.Ability
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose

newtype FatherMateoParallel = FatherMateoParallel InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

fatherMateoParallel :: InvestigatorCard FatherMateoParallel
fatherMateoParallel =
  investigator FatherMateoParallel Cards.fatherMateoParallel
    $ Stats {health = 6, sanity = 8, willpower = 4, intellect = 3, combat = 2, agility = 3}

-- When an investigator would reveal a chaos token: Resolve the token sealed on them instead. Release that token.
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
    ]

instance HasChaosTokenValue FatherMateoParallel where
  getChaosTokenValue iid ElderSign (FatherMateoParallel attrs) | iid == attrs.id = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 1)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage FatherMateoParallel where
  runMessage msg i@(FatherMateoParallel attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> liftRunMessage (Do msg) i
    Do (UseThisAbility iid (isSource attrs -> True) 1) -> do
      investigators <-
        select
          $ affectsOthers
          $ at_ (locationWithInvestigator iid)
          <> not_ (InvestigatorWithSealedChaosToken #bless)
      chooseTargetM iid investigators $ handleTarget iid (attrs.ability 1)
      pure i
    HandleTargetChoice iid (isSource attrs -> True) (InvestigatorTarget iid') -> do
      tokens <- select $ ChaosTokenFaceIs #bless
      focusChaosTokens tokens \unfocus -> do
        chooseTargetM iid tokens \t -> do
          push $ SealChaosToken t
          push $ SealedChaosToken t (toTarget iid')
          push unfocus
      pure i
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      pure i
    ElderSignEffect (is attrs -> True) -> do
      hasBless <- selectAny $ ChaosTokenFaceIs #bless
      if hasBless
        then liftRunMessage (Do $ UseCardAbility attrs.id (toSource attrs) 1 [] NoPayment) i
        else pure i
    _ -> FatherMateoParallel <$> liftRunMessage msg attrs
