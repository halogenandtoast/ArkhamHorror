module Arkham.Asset.Cards.Blackjack2 (
  blackjack2,
  Blackjack2 (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Control.Monad.Trans.Maybe

newtype Blackjack2 = Blackjack2 AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blackjack2 :: AssetCard Blackjack2
blackjack2 =
  asset Blackjack2 Cards.blackjack2

instance HasModifiersFor Blackjack2 where
  getModifiersFor (InvestigatorTarget iid) (Blackjack2 attrs) = do
    mods <- runMaybeT $ do
      (isAbilitySource attrs 1 -> True) <- MaybeT getSkillTestSource
      EnemyTarget eid <- MaybeT getSkillTestTarget
      guardM $ lift $ selectAny $ investigatorEngagedWith eid <> notInvestigator iid
      iid' <- MaybeT getSkillTestInvestigator
      guard $ iid == iid'
      pure $ DamageDealt 1
    pure $ toModifiers attrs $ toList mods
  getModifiersFor _ _ = pure []

instance HasAbilities Blackjack2 where
  getAbilities (Blackjack2 a) = [fightAbility a 1 (ActionCost 1) ControlsThis]

instance RunMessage Blackjack2 where
  runMessage msg a@(Blackjack2 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = toAbilitySource attrs 1
      pushAll
        [ skillTestModifiers source iid [SkillModifier #combat 2, DoesNotDamageOtherInvestigator]
        , chooseFightEnemy iid source #combat
        ]
      pure a
    _ -> Blackjack2 <$> runMessage msg attrs
