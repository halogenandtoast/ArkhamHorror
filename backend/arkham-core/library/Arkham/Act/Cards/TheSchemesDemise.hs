module Arkham.Act.Cards.TheSchemesDemise (
  TheSchemesDemise (..),
  theSchemesDemise,
) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.ChaosBag.RevealStrategy
import Arkham.ChaosToken
import Arkham.DamageEffect
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query (getLead)
import Arkham.Matcher
import Arkham.Message (Message (EnemyDamage))
import Arkham.RequestedChaosTokenStrategy
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
    , mkAbility attrs 3
        $ Objective
        $ forced
        $ EnemyDefeated #after Anyone ByAny
        $ enemyIs Enemies.atlachNacha
    ]

instance RunMessage TheSchemesDemise where
  runMessage msg a@(TheSchemesDemise attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- select $ enemyAtLocationWith iid <> EnemyWithTrait AncientOne
      chooseOrRunOne
        iid
        [targetLabel enemy [EnemyDamage enemy $ nonAttack attrs 3] | enemy <- enemies]
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push (RequestChaosTokens (toSource attrs) (Just iid) (Reveal 1) SetAside)
      pure a
    RequestedChaosTokens source (Just iid) tokens | isSource attrs source -> do
      for_ tokens \token -> do
        ChaosTokenValue _ tokenMod <- getChaosTokenValue iid token.face ()
        case tokenMod of
          NegativeModifier n -> do
            atlachNacha <- selectJust $ IncludeOmnipotent $ enemyIs Enemies.atlachNacha
            push $ HandleAbilityOption iid (toSource atlachNacha) n
          _ -> pure ()
      push $ ResetChaosTokens source
      pure a
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      advanceVia #other attrs attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    _ -> TheSchemesDemise <$> lift (runMessage msg attrs)
