module Arkham.Investigator.Cards.ZoeySamarasParallel (
  zoeySamarasParallel,
  ZoeySamarasParallel (..),
)
where

import Arkham.Ability
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier

newtype ZoeySamarasParallel = ZoeySamarasParallel InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

zoeySamarasParallel :: InvestigatorCard ZoeySamarasParallel
zoeySamarasParallel =
  investigator ZoeySamarasParallel Cards.zoeySamarasParallel
    $ Stats {health = 8, sanity = 6, willpower = 4, intellect = 2, combat = 4, agility = 2}

instance HasAbilities ZoeySamarasParallel where
  getAbilities (ZoeySamarasParallel a) =
    [ playerLimit PerPhase
        $ restrictedAbility a 1 (Self <> HasRemainingBlessTokens)
        $ freeReaction
        $ EnemyDealtDamage #after AnyDamageEffect AnyEnemy
        $ SourceOwnedBy You
    , playerLimit PerRound
        $ restrictedAbility
          a
          2
          (Self <> DuringSkillTest (YourSkillTest #attacking <> SkillTestBeforeRevealingChaosTokens))
        $ FastAbility
        $ ReturnChaosTokensToPoolCost 3 #bless
    ]

instance HasChaosTokenValue ZoeySamarasParallel where
  getChaosTokenValue iid ElderSign (ZoeySamarasParallel attrs) | iid == toId attrs = do
    n <- selectCount $ ChaosTokenFaceIs #bless
    pure $ ChaosTokenValue ElderSign (PositiveModifier n)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage ZoeySamarasParallel where
  runMessage msg i@(ZoeySamarasParallel attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      push $ AddChaosToken #bless
      pure i
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      nextSkillTestModifier (attrs.ability 2) iid (DamageDealt 1)
      pure i
    _ -> ZoeySamarasParallel <$> liftRunMessage msg attrs
