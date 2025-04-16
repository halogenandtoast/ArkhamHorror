module Arkham.Act.Cards.TheSchemesDemise (theSchemesDemise) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.ChaosToken
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (AncientOne))

newtype TheSchemesDemise = TheSchemesDemise ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSchemesDemise :: ActCard TheSchemesDemise
theSchemesDemise = act (3, A) TheSchemesDemise Cards.theSchemesDemise Nothing

instance HasAbilities TheSchemesDemise where
  getAbilities (TheSchemesDemise attrs) =
    [ restrictedAbility attrs 1 (exists $ at_ YourLocation <> EnemyWithTrait AncientOne)
        $ actionAbilityWithCost (ClueCost $ Static 1)
    , mkAbility attrs 2 $ forced $ PhaseBegins #when #mythos
    , mkAbility attrs 3 $ Objective $ forced $ EnemyDefeated #after Anyone ByAny "Atlach-Nacha"
    ]

instance RunMessage TheSchemesDemise where
  runMessage msg a@(TheSchemesDemise attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- select $ enemyAtLocationWith iid <> withTrait AncientOne
      chooseOrRunOneM iid $ targets enemies $ nonAttackEnemyDamage (Just iid) (attrs.ability 1) 3
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      requestChaosTokens iid (attrs.ability 2) 1
      pure a
    RequestedChaosTokens (isAbilitySource attrs 2 -> True) (Just iid) tokens -> do
      for_ tokens \token -> do
        ChaosTokenValue _ tokenMod <- getChaosTokenValue iid token.face ()
        case tokenMod of
          NegativeModifier n -> do
            atlachNacha <- selectJust $ IncludeOmnipotent "Atlach-Nacha"
            push $ HandleAbilityOption iid (toSource atlachNacha) n
          _ -> pure ()
      resetChaosTokens (attrs.ability 2)
      pure a
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      advanceVia #other attrs attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    _ -> TheSchemesDemise <$> liftRunMessage msg attrs
