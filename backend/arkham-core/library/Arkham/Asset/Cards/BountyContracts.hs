module Arkham.Asset.Cards.BountyContracts (
  bountyContracts,
  BountyContracts (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding (EnemyDefeated)
import Arkham.Enemy.Types (Field (..))
import Arkham.Id
import Arkham.Matcher
import Arkham.Projection
import Arkham.Token qualified as Token
import Arkham.Window (Window, windowType)
import Arkham.Window qualified as Window

newtype BountyContracts = BountyContracts AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bountyContracts :: AssetCard BountyContracts
bountyContracts = asset BountyContracts Cards.bountyContracts

instance HasAbilities BountyContracts where
  getAbilities (BountyContracts a) =
    [ restrictedAbility a 1 (available <> ControlsThis) $ freeReaction $ EnemyEntersPlay #after AnyEnemy
    , restrictedAbility a 2 ControlsThis $ ForcedAbility $ EnemyDefeated #after You ByAny EnemyWithBounty
    ]
   where
    available = if hasUses a then mempty else Never

getEnemy :: [Window] -> EnemyId
getEnemy = \case
  ((windowType -> Window.EnemySpawns eid _) : _) -> eid
  ((windowType -> Window.EnemyDefeated _ _ eid) : _) -> eid
  (_ : rest) -> getEnemy rest
  _ -> error "invalid window"

instance RunMessage BountyContracts where
  runMessage msg a@(BountyContracts attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getEnemy -> enemy) _ -> do
      health <- field EnemyHealth enemy
      let maxAmount = min health (min 3 (useCount (assetUses attrs)))
      push
        $ chooseAmounts
          iid
          "Number of bounties to place"
          (MaxAmountTarget maxAmount)
          [("Bounties", (1, maxAmount))]
          (ProxyTarget (toTarget attrs) (toTarget enemy))
      pure a
    ResolveAmounts _ choices (ProxyTarget (isTarget attrs -> True) (EnemyTarget enemy)) -> do
      let bounties = getChoiceAmount "Bounties" choices
      pushAll
        [ SpendUses (toTarget attrs) Bounty bounties
        , PlaceTokens (toAbilitySource attrs 1) (toTarget enemy) Token.Bounty bounties
        ]
      pure a
    UseCardAbility iid (isSource attrs -> True) 2 (getEnemy -> enemy) _ -> do
      bounties <- fieldMap EnemyTokens (Token.countTokens Token.Bounty) enemy
      push $ TakeResources iid bounties (toAbilitySource attrs 2) False
      pure a
    _ -> BountyContracts <$> runMessage msg attrs
