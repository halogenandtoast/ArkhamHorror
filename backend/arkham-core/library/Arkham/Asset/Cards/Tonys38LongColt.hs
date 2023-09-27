module Arkham.Asset.Cards.Tonys38LongColt (
  tonys38LongColt,
  Tonys38LongColt (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Enemy.Types (Field (..))
import Arkham.Matcher hiding (EnemyDefeated)
import Arkham.Matcher qualified as Matcher
import Arkham.Projection
import Arkham.Token qualified as Token

newtype Tonys38LongColt = Tonys38LongColt AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tonys38LongColt :: AssetCard Tonys38LongColt
tonys38LongColt = asset Tonys38LongColt Cards.tonys38LongColt

-- Spend 1 ammo: Fight. You get +1  for each bounty on the attacked enemy. This attack deals +1 damage. If this attack defeats an enemy with 1 or more bounties on it, place 1 bounty on Bounty Contracts.

instance HasAbilities Tonys38LongColt where
  getAbilities (Tonys38LongColt a) =
    [ restrictedAbility
        a
        1
        (ControlsThis <> exists (You <> HandWith (HasCard $ cardIs Cards.tonys38LongColt)))
        $ freeReaction
        $ Matcher.PlayCard #after You
        $ (BasicCardMatch $ CardWithId $ toCardId a)
    , fightAbility a 2 (assetUseCost a Ammo 1) ControlsThis
    ]

instance HasModifiersFor Tonys38LongColt where
  getModifiersFor (InvestigatorTarget iid) (Tonys38LongColt a) | a `controlledBy` iid = do
    mBounties <- runMaybeT $ do
      EnemyTarget eid <- MaybeT getSkillTestTarget
      source <- MaybeT getSkillTestSource
      guard $ isAbilitySource a 2 source
      lift $ fieldMap EnemyTokens (Token.countTokens Token.Bounty) eid
    pure $ toModifiers a [SkillModifier #combat bounties | bounties <- toList mBounties, bounties > 0]
  getModifiersFor _ _ = pure []

instance RunMessage Tonys38LongColt where
  runMessage msg a@(Tonys38LongColt attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      mcard <-
        selectOne $ InHandOf (InvestigatorWithId iid) <> BasicCardMatch (cardIs Cards.tonys38LongColt)
      for_ mcard $ \card -> do
        push $ putCardIntoPlay iid card
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      let source = toAbilitySource attrs 2
      pushAll
        [ skillTestModifier source iid (DamageDealt 1)
        , chooseFightEnemy iid source #combat
        ]
      pure a
    EnemyDefeated eid _ (isAbilitySource attrs 2 -> True) _ -> do
      hadBounty <- eid <=~> EnemyWithBounty
      when hadBounty $ do
        bountyContracts <- selectJust $ assetIs Assets.bountyContracts
        push $ AddUses bountyContracts Bounty 1
      pure a
    _ -> Tonys38LongColt <$> runMessage msg attrs
